describe('Drawing a tile', () => {
  beforeEach(() => cy.startSoloGame())

  it('draws a tile and hands the turn back after the AI plays', () => {
    cy.contains('button', 'Draw a tile').click()
    cy.get('.rack .tile', { timeout: 20000 }).should('have.length', 15)
    cy.contains('Your turn', { timeout: 20000 }).should('exist')
  })

  it('disables Draw once a tile is on the board, until the board is reset', () => {
    cy.contains('button', 'Draw a tile').should('not.be.disabled')
    cy.get('.rack .tile').first().dragTo('.board .new-zone')
    cy.contains('button', 'Draw a tile').should('be.disabled')
    cy.contains('button', 'Reset board').click()
    cy.contains('button', 'Draw a tile').should('not.be.disabled')
  })

  it('keeps the rack layout when drawing, adding the new tile as its own row', () => {
    cy.get('.rack .tile').first().dragTo('.rack .new-zone')
    cy.get('.rack .row').should('have.length', 2)

    cy.contains('button', 'Draw a tile').click()

    cy.get('.rack .row', { timeout: 20000 }).should('have.length', 3)
    cy.get('.rack .tile').should('have.length', 15)
  })
})
