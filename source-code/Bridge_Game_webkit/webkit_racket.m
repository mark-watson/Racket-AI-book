/*
 * webkit_racket.m — Objective-C implementation of the webkit-racket C API
 *
 * Creates a macOS Cocoa application with a WKWebView, exposes a
 * JavaScript bridge (window.webkit_racket.invoke), and provides a
 * clean C API for Racket to call via FFI.
 */

#import <Cocoa/Cocoa.h>
#import <WebKit/WebKit.h>
#include <stdlib.h>
#include <string.h>
#include "webkit_racket.h"

/* ── Internal App Structure ──────────────────────────────────── */

typedef struct {
    NSWindow*             window;
    WKWebView*            webview;
    WKUserContentController* content_controller;
    wkr_bridge_callback_t bridge_callback;
    void*                 bridge_userdata;
    NSString*             pending_html;
    NSString*             pending_url;
    NSString*             pending_file;
    int                   width;
    int                   height;
    NSString*             title;
    BOOL                  running;
} wkr_app_internal;

/* ── Bridge Message Handler ──────────────────────────────────── */

@interface WKRBridgeHandler : NSObject <WKScriptMessageHandler>
@property (assign) wkr_app_internal* app;
@end

@implementation WKRBridgeHandler

- (void)userContentController:(WKUserContentController *)controller
      didReceiveScriptMessage:(WKScriptMessage *)message {
    if (![message.name isEqualToString:@"wkr_bridge"]) return;
    if (!self.app->bridge_callback) return;

    NSDictionary *body = message.body;
    if (![body isKindOfClass:[NSDictionary class]]) return;

    NSString *command = body[@"command"];
    NSString *payload = body[@"payload"];
    NSString *callbackId = body[@"callbackId"];

    if (!command) return;
    if (!payload) payload = @"{}";
    if (!callbackId) callbackId = @"0";

    const char *cmd_c = [command UTF8String];
    const char *payload_c = [payload UTF8String];

    const char *result = self.app->bridge_callback(cmd_c, payload_c,
                                                     self.app->bridge_userdata);

    /* Send the result back to JS */
    NSString *resultStr = result ? [NSString stringWithUTF8String:result]
                                : @"null";
    /* Free the malloc'd result from the callback */
    if (result) free((void*)result);

    NSString *js = [NSString stringWithFormat:
        @"window.webkit_racket._resolveCallback('%@', %@);",
        callbackId, resultStr];

    dispatch_async(dispatch_get_main_queue(), ^{
        [self.app->webview evaluateJavaScript:js completionHandler:nil];
    });
}

@end

/* ── Navigation Delegate ─────────────────────────────────────── */

@interface WKRNavigationDelegate : NSObject <WKNavigationDelegate>
@end

@implementation WKRNavigationDelegate

- (void)webView:(WKWebView *)webView
    didFinishNavigation:(WKNavigation *)navigation {
    /* Page loaded successfully */
}

- (void)webView:(WKWebView *)webView
    didFailNavigation:(WKNavigation *)navigation
            withError:(NSError *)error {
    NSLog(@"webkit-racket: navigation failed: %@", error.localizedDescription);
}

@end

/* ── App Delegate ────────────────────────────────────────────── */

@interface WKRAppDelegate : NSObject <NSApplicationDelegate>
@property (assign) wkr_app_internal* app;
@end

@implementation WKRAppDelegate

- (void)applicationDidFinishLaunching:(NSNotification *)notification {
    [self.app->window makeKeyAndOrderFront:nil];
    [NSApp activateIgnoringOtherApps:YES];

    /* Load any pending content */
    if (self.app->pending_html) {
        [self.app->webview loadHTMLString:self.app->pending_html
                                  baseURL:nil];
        self.app->pending_html = nil;
    } else if (self.app->pending_url) {
        NSURL *url = [NSURL URLWithString:self.app->pending_url];
        NSURLRequest *req = [NSURLRequest requestWithURL:url];
        [self.app->webview loadRequest:req];
        self.app->pending_url = nil;
    } else if (self.app->pending_file) {
        NSURL *fileURL = [NSURL fileURLWithPath:self.app->pending_file];
        [self.app->webview loadFileURL:fileURL
               allowingReadAccessToURL:[fileURL URLByDeletingLastPathComponent]];
        self.app->pending_file = nil;
    }
}

- (BOOL)applicationShouldTerminateAfterLastWindowClosed:(NSApplication *)sender {
    return YES;
}

@end

/* ── C API Implementation ────────────────────────────────────── */

/* Bridge JS injected into every page */
static NSString* bridge_js_source(void) {
    return @"\
window.webkit_racket = {\n\
    _callbackId: 0,\n\
    _callbacks: {},\n\
    invoke: function(command, payload) {\n\
        return new Promise(function(resolve, reject) {\n\
            var id = String(++window.webkit_racket._callbackId);\n\
            window.webkit_racket._callbacks[id] = { resolve: resolve, reject: reject };\n\
            var msg = {\n\
                command: command,\n\
                payload: JSON.stringify(payload || {}),\n\
                callbackId: id\n\
            };\n\
            window.webkit.messageHandlers.wkr_bridge.postMessage(msg);\n\
        });\n\
    },\n\
    _resolveCallback: function(id, result) {\n\
        var cb = window.webkit_racket._callbacks[id];\n\
        if (cb) {\n\
            cb.resolve(result);\n\
            delete window.webkit_racket._callbacks[id];\n\
        }\n\
    }\n\
};\n";
}

wkr_app_t wkr_create(const char* title, int width, int height) {
    wkr_app_internal *app = calloc(1, sizeof(wkr_app_internal));
    if (!app) return NULL;

    app->width = width;
    app->height = height;
    app->title = title ? [NSString stringWithUTF8String:title]
                       : @"webkit-racket";

    /* Set up the user content controller with bridge script */
    app->content_controller = [[WKUserContentController alloc] init];

    WKUserScript *bridgeScript = [[WKUserScript alloc]
        initWithSource:bridge_js_source()
         injectionTime:WKUserScriptInjectionTimeAtDocumentStart
      forMainFrameOnly:YES];
    [app->content_controller addUserScript:bridgeScript];

    /* Register the message handler */
    WKRBridgeHandler *handler = [[WKRBridgeHandler alloc] init];
    handler.app = app;
    [app->content_controller addScriptMessageHandler:handler
                                                name:@"wkr_bridge"];

    /* WebView configuration */
    WKWebViewConfiguration *config = [[WKWebViewConfiguration alloc] init];
    config.userContentController = app->content_controller;

    /* Allow developer extras (Web Inspector) in debug builds */
#ifdef DEBUG
    [config.preferences setValue:@YES forKey:@"developerExtrasEnabled"];
#endif

    /* Create the WebView */
    NSRect frame = NSMakeRect(0, 0, width, height);
    app->webview = [[WKWebView alloc] initWithFrame:frame
                                       configuration:config];
    app->webview.autoresizingMask = NSViewWidthSizable | NSViewHeightSizable;

    /* Navigation delegate */
    WKRNavigationDelegate *navDelegate = [[WKRNavigationDelegate alloc] init];
    app->webview.navigationDelegate = navDelegate;

    /* Create the window */
    NSUInteger styleMask = NSWindowStyleMaskTitled
                         | NSWindowStyleMaskClosable
                         | NSWindowStyleMaskMiniaturizable
                         | NSWindowStyleMaskResizable;

    app->window = [[NSWindow alloc]
        initWithContentRect:frame
                  styleMask:styleMask
                    backing:NSBackingStoreBuffered
                      defer:NO];
    [app->window setTitle:app->title];
    [app->window center];
    [app->window setContentView:app->webview];

    return (wkr_app_t)app;
}

void wkr_run(wkr_app_t handle) {
    if (!handle) return;
    wkr_app_internal *app = (wkr_app_internal*)handle;

    @autoreleasepool {
        NSApplication *nsApp = [NSApplication sharedApplication];
        [nsApp setActivationPolicy:NSApplicationActivationPolicyRegular];

        /* Create a basic menu bar */
        NSMenu *menuBar = [[NSMenu alloc] init];
        NSMenuItem *appMenuItem = [[NSMenuItem alloc] init];
        [menuBar addItem:appMenuItem];

        NSMenu *appMenu = [[NSMenu alloc] init];
        NSMenuItem *quitItem = [[NSMenuItem alloc]
            initWithTitle:[NSString stringWithFormat:@"Quit %@", app->title]
                   action:@selector(terminate:)
            keyEquivalent:@"q"];
        [appMenu addItem:quitItem];
        [appMenuItem setSubmenu:appMenu];
        [nsApp setMainMenu:menuBar];

        /* Set the app delegate */
        WKRAppDelegate *delegate = [[WKRAppDelegate alloc] init];
        delegate.app = app;
        [nsApp setDelegate:delegate];

        app->running = YES;
        [nsApp run];
        app->running = NO;
    }
}

void wkr_quit(wkr_app_t handle) {
    if (!handle) return;
    dispatch_async(dispatch_get_main_queue(), ^{
        [NSApp terminate:nil];
    });
}

void wkr_destroy(wkr_app_t handle) {
    if (!handle) return;
    wkr_app_internal *app = (wkr_app_internal*)handle;
    /* ARC handles Objective-C object cleanup */
    free(app);
}

void wkr_load_html(wkr_app_t handle, const char* html) {
    if (!handle || !html) return;
    wkr_app_internal *app = (wkr_app_internal*)handle;
    NSString *htmlStr = [NSString stringWithUTF8String:html];

    if (app->running) {
        dispatch_async(dispatch_get_main_queue(), ^{
            [app->webview loadHTMLString:htmlStr baseURL:nil];
        });
    } else {
        app->pending_html = htmlStr;
    }
}

void wkr_load_url(wkr_app_t handle, const char* url) {
    if (!handle || !url) return;
    wkr_app_internal *app = (wkr_app_internal*)handle;
    NSString *urlStr = [NSString stringWithUTF8String:url];

    if (app->running) {
        dispatch_async(dispatch_get_main_queue(), ^{
            NSURL *nsUrl = [NSURL URLWithString:urlStr];
            NSURLRequest *req = [NSURLRequest requestWithURL:nsUrl];
            [app->webview loadRequest:req];
        });
    } else {
        app->pending_url = urlStr;
    }
}

void wkr_load_file(wkr_app_t handle, const char* path) {
    if (!handle || !path) return;
    wkr_app_internal *app = (wkr_app_internal*)handle;
    NSString *pathStr = [NSString stringWithUTF8String:path];

    /* Resolve relative paths */
    if (![pathStr isAbsolutePath]) {
        NSString *cwd = [[NSFileManager defaultManager] currentDirectoryPath];
        pathStr = [cwd stringByAppendingPathComponent:pathStr];
    }

    if (app->running) {
        dispatch_async(dispatch_get_main_queue(), ^{
            NSURL *fileURL = [NSURL fileURLWithPath:pathStr];
            [app->webview loadFileURL:fileURL
                  allowingReadAccessToURL:[fileURL URLByDeletingLastPathComponent]];
        });
    } else {
        app->pending_file = pathStr;
    }
}

void wkr_eval_js(wkr_app_t handle, const char* js) {
    if (!handle || !js) return;
    wkr_app_internal *app = (wkr_app_internal*)handle;
    NSString *jsStr = [NSString stringWithUTF8String:js];

    dispatch_async(dispatch_get_main_queue(), ^{
        [app->webview evaluateJavaScript:jsStr completionHandler:nil];
    });
}

void wkr_set_bridge_callback(wkr_app_t handle,
                               wkr_bridge_callback_t callback,
                               void* userdata) {
    if (!handle) return;
    wkr_app_internal *app = (wkr_app_internal*)handle;
    app->bridge_callback = callback;
    app->bridge_userdata = userdata;
}

void wkr_set_title(wkr_app_t handle, const char* title) {
    if (!handle || !title) return;
    wkr_app_internal *app = (wkr_app_internal*)handle;
    NSString *titleStr = [NSString stringWithUTF8String:title];

    dispatch_async(dispatch_get_main_queue(), ^{
        [app->window setTitle:titleStr];
    });
}

void wkr_set_size(wkr_app_t handle, int width, int height) {
    if (!handle) return;
    wkr_app_internal *app = (wkr_app_internal*)handle;

    dispatch_async(dispatch_get_main_queue(), ^{
        NSRect frame = [app->window frame];
        NSRect newFrame = NSMakeRect(frame.origin.x, frame.origin.y,
                                     width, height);
        [app->window setFrame:newFrame display:YES animate:YES];
    });
}

void wkr_set_resizable(wkr_app_t handle, int resizable) {
    if (!handle) return;
    wkr_app_internal *app = (wkr_app_internal*)handle;

    dispatch_async(dispatch_get_main_queue(), ^{
        NSUInteger mask = [app->window styleMask];
        if (resizable) {
            mask |= NSWindowStyleMaskResizable;
        } else {
            mask &= ~NSWindowStyleMaskResizable;
        }
        [app->window setStyleMask:mask];
    });
}
