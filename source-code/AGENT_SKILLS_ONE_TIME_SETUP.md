# One-Time Setup: Install Agent Skills

This guide covers how to install the Racket AI book API skills for different coding agents.

## gemini-cli

Copy the skill file and link it for global use:

```
mkdir -p ~/.gemini/skills/racket-ai-dev
cp source-code/AGENT_SKILLS_README.md ~/.gemini/skills/racket-ai-dev/SKILL.md
pushd ~/.gemini/skills/
gemini skills link
popd
```

Verify it is installed:

```
gemini skills list
```

## Claude Code

Claude Code uses **CLAUDE.md** files for project-level context. To give Claude access to the Racket book APIs:

1. Copy or symlink the skill reference into your project root:

```bash
cp source-code/AGENT_SKILLS_README.md /path/to/your/project/CLAUDE.md
```

Or, if you want to keep it alongside other instructions, append it:

```bash
cat source-code/AGENT_SKILLS_README.md >> /path/to/your/project/CLAUDE.md
```

2. Alternatively, create a `.claude/skills/` directory and place the file there:

```bash
mkdir -p ~/.claude/skills/racket-ai-dev
cp source-code/AGENT_SKILLS_README.md ~/.claude/skills/racket-ai-dev/SKILL.md
```

Claude Code will automatically read `CLAUDE.md` and any markdown files in `.claude/` at the start of each session.

## Hermes Agent

Hermes Agent by Nous Research stores reusable skills in `~/.hermes/skills/`. Copy the API reference there:

```bash
mkdir -p ~/.hermes/skills/racket-ai-dev
cp source-code/AGENT_SKILLS_README.md ~/.hermes/skills/racket-ai-dev/SKILL.md
```

Hermes will automatically discover files in its `skills/` directory and use them as context when generating code. You might need to one time ask Hermes a question like "what SKILLs do I have for Racket?"

## Google Antigravity

In each project directory:

Place the **source-code/AGENT_SKILLS_README.md** file inside a **.gemini/skills/racket-ai-dev/** directory at the root of the project you have open in Antigravity.
