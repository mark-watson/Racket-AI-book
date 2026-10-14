/*
 * webkit_racket.h — C API for webkit-racket
 *
 * A minimal C interface to macOS Cocoa + WKWebView, designed to be
 * called from Racket via FFI.
 *
 * Thread safety: All functions must be called from the main thread.
 */

#ifndef WEBKIT_HASKELL_H
#define WEBKIT_HASKELL_H

#ifdef __cplusplus
extern "C" {
#endif

/* Opaque handle to a webkit-racket application instance */
typedef void* wkr_app_t;

/*
 * Callback type for bridge invocations from JavaScript.
 *
 * When JS calls window.webkit_racket.invoke(command, payload),
 * this callback fires.
 *
 * The callback must return a malloc'd JSON string (the bridge will free it),
 * or NULL to indicate no response / error.
 */
typedef const char* (*wkr_bridge_callback_t)(const char* command,
                                               const char* payload,
                                               void* userdata);

/* ── Lifecycle ───────────────────────────────────────────────── */

wkr_app_t wkr_create(const char* title, int width, int height);
void wkr_run(wkr_app_t app);
void wkr_quit(wkr_app_t app);
void wkr_destroy(wkr_app_t app);

/* ── Content Loading ─────────────────────────────────────────── */

void wkr_load_html(wkr_app_t app, const char* html);
void wkr_load_url(wkr_app_t app, const char* url);
void wkr_load_file(wkr_app_t app, const char* path);

/* ── JavaScript ──────────────────────────────────────────────── */

void wkr_eval_js(wkr_app_t app, const char* js);

/* ── Bridge ──────────────────────────────────────────────────── */

void wkr_set_bridge_callback(wkr_app_t app,
                               wkr_bridge_callback_t callback,
                               void* userdata);

/* ── Window Management ───────────────────────────────────────── */

void wkr_set_title(wkr_app_t app, const char* title);
void wkr_set_size(wkr_app_t app, int width, int height);
void wkr_set_resizable(wkr_app_t app, int resizable);

#ifdef __cplusplus
}
#endif

#endif /* WEBKIT_HASKELL_H */
