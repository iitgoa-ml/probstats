// my_script.js
Shiny.addCustomMessageHandler('jsHandler', function(message) {
    alert('JavaScript says: ' + message);
});
