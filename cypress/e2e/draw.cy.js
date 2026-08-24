describe('Drawing a tile', () => {
  it('draws a tile and hands the turn back after the AI plays', () => {
    cy.startSoloGame()

    cy.contains('button', 'Draw a tile').click()

    // The player gains the drawn tile; once the AI has taken its turn it is the
    // player's turn again.
    cy.get('.rack .tile', { timeout: 20000 }).should('have.length', 15)
    cy.contains('Your turn', { timeout: 20000 }).should('exist')
  })
})
