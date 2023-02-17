/* Script from https://stackoverflow.com/q/75487336/7612904, Marcus Adams */

$(document).ready(function() {
  const elements = document.querySelectorAll('.element')
  
  elements.forEach(element => {
    element.addEventListener('click', () => Shiny.onInputChange("SelectedElement", element.id))
  })
  
})