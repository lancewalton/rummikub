describe('Off-turn board is read-only', () => {
  beforeEach(() => cy.startSoloGame())

  it('disables actions and rejects board changes, but leaves the rack editable', () => {
    // Re-inject the real game state with a different player to move.
    cy.window().then((win) => {
      const view = win.__lastView
      const other = view.players.find((player) => player.id !== view.you).id
      const offTurn = { GameState: { view: { ...view, currentPlayer: other } } }
      win.__sock.dispatchEvent(new win.MessageEvent('message', { data: JSON.stringify(offTurn) }))
    })

    cy.contains('Waiting for').should('exist')
    cy.contains('button', 'Reset').should('be.disabled')
    cy.contains('button', 'Commit move').should('be.disabled')
    cy.contains('button', 'Draw a tile').should('be.disabled')

    // Dragging a rack tile onto the board does nothing while it is not your turn.
    cy.get('.rack .tile').first().dragTo('.board .new-zone')
    cy.get('.board .tile').should('have.length', 0)

    // The rack itself can still be rearranged.
    cy.get('.rack .row').should('have.length', 1)
    cy.get('.rack .tile').first().dragTo('.rack .new-zone')
    cy.get('.rack .row').should('have.length', 2)
  })
})
