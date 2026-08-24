describe('Lobby', () => {
  it('joins, adds an AI opponent, and starts a game', () => {
    cy.visit('/')
    cy.get('h1').should('contain', 'Rummikub')

    cy.joinAs('Alice')
    cy.get('h2').should('contain', 'Lobby')
    cy.get('li').should('contain', 'Alice')

    cy.contains('button', 'Add AI player').click()
    cy.get('li').should('contain', 'Bot (AI)')

    cy.contains('button', 'Start game').click()
    cy.get('h2').should('contain', 'Game')
    cy.get('.rack .tile').should('have.length', 14)
    cy.get('.board .row').should('have.length', 0)
    cy.contains('button', 'Commit move').should('be.disabled')
  })
})
