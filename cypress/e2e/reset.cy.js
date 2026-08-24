describe('Reset board', () => {
  beforeEach(() => cy.startSoloGame())

  it('clears the board but keeps the rack organisation', () => {
    // Organise the rack into two rows.
    cy.get('.rack .tile').first().dragTo('.rack .new-zone')
    cy.get('.rack .row').should('have.length', 2)

    // Move a tile up to the board.
    cy.get('.rack .tile').first().dragTo('.board .new-zone')
    cy.get('.board .tile').should('have.length', 1)
    cy.get('.rack .tile').should('have.length', 13)

    cy.contains('button', 'Reset board').click()

    // The board is cleared, the two organised rows survive, and the played
    // tile comes back to the rack (in a new row) — 14 tiles again.
    cy.get('.board .tile').should('have.length', 0)
    cy.get('.rack .tile').should('have.length', 14)
    cy.get('.rack .row').should('have.length', 3)
  })
})
