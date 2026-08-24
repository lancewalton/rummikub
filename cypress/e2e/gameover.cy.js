describe('Game over and play again', () => {
  beforeEach(() => cy.startSoloGame())

  it('announces the winner, disables Commit and offers a rematch', () => {
    cy.injectServerMessage({ GameOver: { winner: { id: 'bot-1', name: 'Bot', isAi: true, tileCount: 0 } } })

    cy.contains('Game over — Bot wins!').should('exist')
    cy.contains('button', 'Commit move').should('be.disabled')
    cy.contains('button', 'Play again').should('exist').click()

    cy.window().its('__sent').should((sent) => {
      expect(sent.some((message) => message.includes('PlayAgain'))).to.equal(true)
    })
  })

  it('clears the game-over banner when a new game starts', () => {
    cy.injectServerMessage({ GameOver: { winner: null } })
    cy.contains('Game over — a draw.').should('exist')

    cy.injectServerMessage({ GameStarted: {} })
    cy.contains('Game over').should('not.exist')
    cy.contains('button', 'Play again').should('not.exist')
  })
})
