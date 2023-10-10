// Carrega dados
var file = new File("./data-years.json");
file.open("r");
var content = file.read();
file.close();
var data = JSON.parse(content).data;

// Cria diálogo para seleção de duração de cada ano
var dialog = new Window("dialog", "");
var group = dialog.add("group");
group.orientation = "column";

var labelName = group.add("statictext", undefined, "Quantos segundos dura cada ano:");
var numericInput = group.add("edittext", undefined, "1");
numericInput.characters = 4;

var okButton = group.add("button", undefined, "OK");

// Clique OK do diálogo de seleção
okButton.onClick = function() {
  var selectedMinute = parseFloat(numericInput.text);

  if (isNaN(selectedMinute)) {
    alert("Error: numericInput is NaN");
    return;
  };
  
  dialog.close();
  addSteps(selectedMinute)
};

// Função para remover todos os keyframes de uma propriedade
function removeAllKeyframes(property) {
  while (property.numKeys > 0) {
    property.removeKey(1);
  }
}

// Função para converter valor hexadecimal para RGB normalizado
function hexToRGB(hex) {
  const normal = hex.match(/^#([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})$/i);
  if (normal) return normal.slice(1).map(function(e) {return parseInt(e, 16)/255 });

  return null;
}


// Adiciona steps para cada ano
function addSteps(duration) {
  // Adiciona steps para cada ano
  var layer = app.project.activeItem.layer("ajustes");

  var cor = layer.property("Effects").property("cor").property("Color");
  var ano = layer.property("Effects").property("ano").property("Slider");
  var temperatura = layer.property("Effects").property("temperatura").property("Slider");
  removeAllKeyframes(cor);
  removeAllKeyframes(ano);
  removeAllKeyframes(temperatura);

  for (var i = 0; i < data.length; i++) {
    var keyframeTime = i * duration;

    var corValue = hexToRGB(data[i].color);
    // var corValue = [0.2, 0.4, 0.2];
    var corKeyframe = cor.addKey(keyframeTime);
    cor.setValueAtKey(corKeyframe, corValue);

    //ano
    var anoValue = data[i].year;
    var anoKeyframe = ano.addKey(keyframeTime);
    ano.setValueAtKey(anoKeyframe, anoValue);

    //temperatura
    var temperaturaValue = data[i].temperature;
    var temperaturaKeyframe = temperatura.addKey(keyframeTime);
    temperatura.setValueAtKey(temperaturaKeyframe, temperaturaValue);
  }

}

// Abrir a caixa de diálogo
dialog.show();