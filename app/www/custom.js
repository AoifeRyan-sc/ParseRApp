Shiny.addCustomMessageHandler('toggle-card', function(id) {
  var card = document.getElementById(id);
  if (card) {
    card.classList.toggle('collapsed');
  }
});
