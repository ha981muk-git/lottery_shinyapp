---
name: UI UX Retention Upgrade
description: "Improve Shiny UI and UX to increase repeat visits, user trust, and client confidence."
argument-hint: "What flow or screen should be improved, and what business outcome matters most?"
agent: "agent"
---
Use this prompt to design and implement a focused UI and UX upgrade that helps users return and helps clients trust the product.

Treat the prompt argument as the target area and outcome. Example arguments:
- "Make the date filter and metric exploration feel easier for first-time users"
- "Improve mobile dashboard readability so users check results more often"
- "Increase confidence in insights with clearer context and explanation"

Project context:
- UI entry and shared controls: [DashboardModule.R](../../DashboardModule.R)
- App entry point: [App.R](../../App.R)
- Metric modules: [dashboard](../../dashboard)
- Styles and behavior: [www/custom.css](../../www/custom.css), [www/Home.css](../../www/Home.css), [www/custom.js](../../www/custom.js)
- Localization keys: [translations.R](../../translations.R)

Requirements:
1. Inspect the current UI flow in the relevant modules before changing code.
2. Propose 2 to 3 high-impact improvements tied to return usage or engagement.
3. Implement the best option directly in code, preserving current Shiny module patterns.
4. Keep all user-facing text localized with t(key, lang), adding new keys to both en and de.
5. Preserve existing app behavior and filtering logic unless changes are explicitly justified.
6. Ensure the result works on desktop and mobile.
7. Keep changes cohesive and avoid broad unrelated refactors.

Output format:
1. Retention goal and 2 to 3 UX options with quick trade-offs.
2. Chosen option and why it is best for repeat visits.
3. Exact code changes made, with file-by-file bullets.
4. Why each change should increase repeat visits or trust.
5. Quick validation steps with commands to run locally.
6. Optional next experiment ideas (max 3).

Execution style:
- Start with concise options, then implement the chosen option.
- Prefer polished, intentional visual design over generic defaults.
- If requirements conflict, prioritize correctness, localization, and usability.
