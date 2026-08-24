describe('Board validity', () => {
  beforeEach(() => cy.startSoloGame())

  it('keeps Commit disabled while the board is empty', () => {
    cy.get('.board .row').should('have.length', 0)
    cy.contains('button', 'Commit move').should('be.disabled')
  })

  it('flags a too-short group as invalid and keeps Commit disabled', () => {
    // Any two tiles form a group that is too short to be a valid run or set.
    cy.get('.rack .tile').eq(0).dragTo('.board .new-zone')
    cy.get('.rack .tile').eq(0).dragTo('.board .row')

    cy.get('.board .row').should('have.length', 1)
    cy.get('.board .tile').should('have.length', 2)
    cy.get('.board .invalid').should('exist')
    cy.contains('button', 'Commit move').should('be.disabled')
  })
})
