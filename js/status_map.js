function handle_render() {
    improve_search_button();
    add_layer_control_title('Inspect AQSU PurpleAir:');
    convert_layer_control_to_radio_buttons();
};

// Convert layer control to radio buttons (so only one selected at a time) # TODO: move to aqmapr
function convert_layer_control_to_radio_buttons() {
    inputs = document.getElementsByClassName('leaflet-control-layers-selector');
    for (let i = 0; i < inputs.length; i++) {
        inputs[i].name = 'n';
        inputs[i].type = 'radio';
    }
}

// JS to improve search icon formatting to match leaflet controls
// TODO: submit PR to leaflet.extras
function improve_search_button() {
    document.getElementsByClassName('leaflet-control-search')[0].className = 'leaflet-bar easy-button-container leaflet-control leaflet-control-search';
    document.getElementsByClassName('leaflet-control-search')[0].style.boxShadow = 'none';
    document.getElementsByClassName('leaflet-control-search')[0].style.backgroundColor = null;
    document.getElementsByClassName('search-button')[0].style.backgroundImage = 'url(icons/search_icon.jif)';
    document.getElementsByClassName('search-button')[0].style.backgroundPosition = '8px center';
    document.getElementsByClassName('search-button')[0].style.backgroundSize = '14px';
    document.getElementsByClassName('search-button')[0].style.backgroundColor = 'rgba(255,255,255,.8)';
    document.getElementsByClassName('search-button')[0].title = 'Search Monitors...';
    document.getElementsByClassName('search-tooltip')[0].style.marginTop = '1px';
}

// TODO: improve and move to aqmapr
function add_layer_control_title(title) {
    document.getElementsByClassName('leaflet-control-layers-overlays')[0].style.fontSize = 'larger';
    document.getElementsByClassName('leaflet-control-layers-overlays')[0].prepend(title);
}

function change_plot(element) {
    img = element.parentElement.childNodes[0];
    is_flag_plot = img.src.match(/cleaned/g) === null;
    if (is_flag_plot) {
        img.src = img.src.replace('.png', '_cleaned.png');
    } else {
        img.src = img.src.replace('_cleaned.png', '.png');
    }
}
