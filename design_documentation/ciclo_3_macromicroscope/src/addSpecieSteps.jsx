// Carrega dados
var file = new File("./data.json");
file.open("r");
var content = file.read();
file.close();
var jsonData = JSON.parse(content).data;

// Cria diálogo para seleção de espécie
var options = jsonData.map(function(s) {return s.nomeVulgar})
var dialog = new Window("dialog", "");
var group = dialog.add("group");
group.orientation = "column";

var labelName = group.add("statictext", undefined, "Selecione a espécie:");
var dropdownName = group.add("dropdownlist", undefined, options);
dropdownName.selection = 0;

var labelName = group.add("statictext", undefined, "Quantos segundos dura cada ano:");
var numericInput = group.add("edittext", undefined, "60");
numericInput.characters = 4;

var okButton = group.add("button", undefined, "OK");

// Clique OK do diálogo de seleção
okButton.onClick = function() {
  var selectedSpecie = dropdownName.selection.index;
  var selectedMinute = parseFloat(numericInput.text);

  if (isNaN(selectedMinute)) {
    alert("Error: numericInput is NaN");
    return;
  };
  
  dialog.close();
  addSteps(selectedSpecie, selectedMinute)
};

// Função para remover todos os keyframes de uma propriedade
function removeAllKeyframes(property) {
  while (property.numKeys > 0) {
    property.removeKey(1);
  }
}

// Adiciona steps para a espécie selecionada
function addSteps(index, duration) {
  var specieData = jsonData[index];
  var layer = app.project.activeItem.layer("ajustes");
  
  var seed = layer.property("Effects").property("seed").property("Slider");
  seed.setValue(index);

  var populacao = layer.property("Effects").property("populacao").property("Slider");
  var pontos = layer.property("Effects").property("pontos").property("Slider");
  removeAllKeyframes(populacao);
  removeAllKeyframes(pontos);

  for (var i = 0; i < specieData.populacao.length; i++) {
    var keyframeTime = i * duration;

    // populacao - dividindo por 1.000 devido a limitações de maxValue do slider
    var populacaoValue = specieData.populacao[i] / 1000;
    var populacaoKeyframe = populacao.addKey(keyframeTime);
    populacao.setValueAtKey(populacaoKeyframe, populacaoValue);

    //pontos
    var pontosValue = specieData.pontos[i];
    var pontosKeyframe = pontos.addKey(keyframeTime);
    pontos.setValueAtKey(pontosKeyframe, pontosValue);
  }

  // Adiciona textos
  var nomeVulgarLayer = app.project.activeItem.layer("nome").property("Source Text");
  nomeVulgarLayer.expression = "'" + specieData.nomeVulgar + "'";

  var nomeCientificoLayer = app.project.activeItem.layer("nome-cientifico").property("Source Text");
  nomeCientificoLayer.expression = "'" + specieData.nomeCientifico + "'";

}

// Abrir a caixa de diálogo
dialog.show();