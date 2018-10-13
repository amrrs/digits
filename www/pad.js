shinyjs.init = function() {

var signaturePad = new SignaturePad(document.getElementById('signature-pad'), {
  backgroundColor: 'rgb(255, 255, 255)',
  penColor: '#337AB7',
  maxWidth: 10,
  minWidth: 9
});
var saveButton = document.getElementById('save');
var cancelButton = document.getElementById('clear');

saveButton.addEventListener('click', function (event) {
  var data = signaturePad.toDataURL('image/png');
    Shiny.onInputChange('source', data);
});

cancelButton.addEventListener('click', function (event) {
  signaturePad.clear();
});

}
