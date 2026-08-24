# End-to-end tests (Cypress)

These drive the real web GUI in a browser against a running server.

## Running

1. Build the frontend and start the backend (serves the built JS + the `/ws`
   WebSocket on port 8080):

   ```bash
   sbt "frontend/fastLinkJS"
   sbt "backend/run"        # leave running in another terminal
   ```

2. Run the suite (or `npm run cypress:open` for the interactive runner):

   ```bash
   npm install              # first time only; then: npx cypress install
   npm run cypress:run
   ```

## What's covered

- **lobby** — join → add an AI → start; the game view renders with a 14-tile rack.
- **arrange** — split a tile onto a new rack row; move a tile up to a board group.
- **validity** — an empty board and a too-short group both keep *Commit* disabled;
  an invalid board group shows the `✗ invalid` indicator.
- **draw** — drawing a tile hands the turn to the AI and back.
- **gameover** — a `GameOver` shows the winner/`Play again`; a new `GameStarted`
  clears the banner.
- **offturn** — when it is not your turn, the action buttons are disabled and the
  board rejects drops, while the rack stays arrangeable.

## Techniques

- **Drag-and-drop**: the app reacts to native HTML5 drag events, so `dragTo`
  dispatches `dragstart`/`dragover`/`drop` with a shared `DataTransfer`
  (`cypress/support/commands.js`).
- **Hard-to-reach states**: game-over and off-turn are reached by injecting a
  server message straight into the app's captured WebSocket, rather than playing
  a full game to completion.
