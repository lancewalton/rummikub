describe('Rack arrangement', () => {
  beforeEach(() => cy.startSoloGame())

  it('splits a tile onto a new rack row', () => {
    cy.get('.rack .row').should('have.length', 1)
    cy.get('.rack .tile').first().dragTo('.rack .new-zone')
    cy.get('.rack .row').should('have.length', 2)
    cy.get('.rack .tile').should('have.length', 14)
  })

  it('moves a tile up to a new board group', () => {
    cy.get('.rack .tile').first().dragTo('.board .new-zone')
    cy.get('.board .row').should('have.length', 1)
    cy.get('.board .tile').should('have.length', 1)
    cy.get('.rack .tile').should('have.length', 13)
  })
})
