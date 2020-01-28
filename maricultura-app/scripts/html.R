test <- HTML( '<p>This tool was created for use by the World Wildlife Fund to assist with marine aquaculture planning within the Exclusive Economic Zone (EEZ) of Brazil. The creators are a group of masters students at the Bren School.....</p>'
)

fish_radiobuttons <- HTML('<div id="selectSpecies" class="form-group shiny-input-radiogroup shiny-input-container">
  <label class="control-label" for="selectSpecies">
    <h3>Species</h3>
  </label>
  <div class="shiny-options-group">
    <div class="radio">
      <label>
        <input type="radio" name="radio" value="Salmo salar"/>
        <span>Atlantic salmon (<i>Salmo salar</i>)<br><img src="atlantic_salmon.png" alt=“image of salmon“ height="100px"/></span>
      </label>
    </div>
    <div class="radio">
      <label>
        <input type="radio" name="radio" value="Sparus aurata"/>
        <span>gilthead seabream (<i>Sparus aurata</i>)<br><br><img src="seabream.png" alt=“image of salmon“ height="70px"/></span>
      </label>
    </div>
    <div class="radio">
      <label>
        <input type="radio" name="radio" value="Rachycentron
canadum" checked="checked"/>
        <span>cobia (<i>Rachycentron canadum</i>)<br><img src="cobia.png" alt=“image of salmon“  height="100px"/></span>
      </label>
    </div>
  </div>
</div>')

