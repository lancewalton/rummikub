describe('Lobby', () => {
  it('creates a game, shows a shareable code, adds an AI and starts', () => {
    cy.visit('/')
    cy.get('h1').should('contain', 'Rummikub')

    cy.createGame('Alice')
    cy.get('h2').should('contain', 'Lobby')
    cy.contains(/Game code: [A-Z]{4}/).should('exist')
    cy.get('li').should('contain', 'Alice')

    cy.contains('button', 'Add AI player').click()
    cy.get('li').should('contain', 'Bot (AI)')

    cy.contains('button', 'Start game').click()
    cy.get('h2').should('contain', 'Game')
    cy.get('.rack .tile').should('have.length', 14)
    cy.get('.rack .tile').first().should('have.css', 'background-color').and('not.equal', 'rgba(0, 0, 0, 0)')
    cy.contains('button', 'Commit move').should('be.disabled')
  })

  it('reports an unknown game code', () => {
    cy.visit('/')
    cy.get('input[placeholder="Your name"]').type('Bob')
    cy.get('input[placeholder="Game code"]').type('ZZZZ')
    cy.contains('button', 'Join game').click()
    cy.contains('No game found').should('exist')
  })
})
