// Capture the app's WebSocket instance and record everything it sends.
// Patching `send` is enough: the socket is created on page load, but the app
// only sends on user actions, so the first send (Join) hands us the instance.
Cypress.Commands.add('captureSocket', () => {
  cy.window().then((win) => {
    win.__sent = []
    const originalSend = win.WebSocket.prototype.send
    win.WebSocket.prototype.send = function (data) {
      win.__sock = this
      win.__sent.push(data)
      return originalSend.call(this, data)
    }
  })
})

// Record inbound GameState views so a test can read the real player ids.
// Call after the socket has been captured (i.e. after the first send).
Cypress.Commands.add('recordIncoming', () => {
  cy.window().then((win) => {
    win.__sock.addEventListener('message', (event) => {
      const message = JSON.parse(event.data)
      if (message.GameState) win.__lastView = message.GameState.view
    })
  })
})

Cypress.Commands.add('joinAs', (name) => {
  cy.get('input[placeholder="Your name"]').type(name)
  cy.contains('button', 'Join').click()
})

// Visit, join as a human, add one AI opponent and start the game.
Cypress.Commands.add('startSoloGame', (name = 'Alice') => {
  cy.visit('/')
  cy.captureSocket()
  cy.joinAs(name)
  cy.recordIncoming()
  cy.contains('button', 'Add AI player').click()
  cy.contains('button', 'Start game').click()
  cy.get('.rack .tile').should('have.length', 14)
})

// HTML5 drag-and-drop: the app reacts to native drag events, so we dispatch a
// dragstart on the source and a drop on the target sharing one DataTransfer.
Cypress.Commands.add('dragTo', { prevSubject: 'element' }, (subject, targetSelector) => {
  cy.window().then((win) => {
    const dataTransfer = new win.DataTransfer()
    cy.wrap(subject).trigger('dragstart', { dataTransfer, force: true })
    // The drop re-renders the tiles, detaching `subject`, so this is the last
    // step that touches it. The app does not listen for dragend.
    cy.get(targetSelector).first().trigger('dragover', { dataTransfer, force: true }).trigger('drop', { dataTransfer, force: true })
  })
})

// Feed a server message straight into the app's socket (for states that are
// hard to reach through real play, e.g. game over).
Cypress.Commands.add('injectServerMessage', (message) => {
  cy.window().then((win) => {
    win.__sock.dispatchEvent(new win.MessageEvent('message', { data: JSON.stringify(message) }))
  })
})
