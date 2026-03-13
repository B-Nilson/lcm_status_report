function handle_render() {
    improve_search_button();
    add_layer_control_titles(
        ['Inspect Flags for:', 'View Observerations:'],
        [0, 3]
    );
    convert_layer_control_to_radio_buttons();
};
