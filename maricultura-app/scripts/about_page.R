header <- HTML('<div class="jumbotron" style="height: 75vh; display: flex; justify-content: center; align-items: center; background: linear-gradient( rgba(197,219,235, 0.65), rgba(197,219,235, 0.8)),
	url(https://images.unsplash.com/photo-1518837695005-2083093ee35b?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=crop&w=1950&q=80); background-size: cover; background-position: center top; letter-spacing: 0.7em;">
                           <h1 class="display-3" style="text-align: center; color: white; font-size: 5em; font-weight: 550;">SHAPING &nbsp;MARICULTURE &nbsp;IN &nbsp;BRAZIL</h1>
                           </div>')

project <- HTML('<div style="height: 75vh; overflow: auto" class="jumbotron">
                      <h1 class="display-3" >The Project</h1>
                      <div style="display: table-cell; vertical-align: middle;">
  <img style="width: 40vh; height: 40vh; object-fit: cover; margin: 0px 0px 1em 1.5em; padding: 0.5em 0px 0px 0px; float: right; vertical-align: middle" src="6.png" alt="Bren Hall Building">
  <p style="text-align: justify;" class="lead">The Bren School at UC Santa Barbara is a graduate-level environmental science and management program whose students fulfill a 12 month Group Project to solve an environmental problem faced by a real-world client, in our case, the World Wildlife Fund. We explored the spatial feasibility of offshore mariculture in Brazil’s Exclusive Economic Zone, and created a web-based tool to predict yields and profitability for offshore mariculture of various finfish. This app is the interactive component of our overall thesis project.</p>
  <p class="lead">For more information about the Bren School visit their website.</p>
  <p style="display: inline-block; vertical-align: bottom" class="lead">
    <a class="btn btn-primary btn-lg" href=https://www.bren.ucsb.edu target="_blank" role="button">Learn more</a>
  </p>
  </div>
</div>'
)

creators <- HTML('<div style="height: 75vh; overflow: auto" class="jumbotron">
  <h1 class="display-3"> Meet the Creators</h1>
  <div>
  <img style="width: 40vh; height: 40vh; object-fit: cover;  margin: 0px 0px 1em 1.5em; padding: 0.5em 0px 0px 0px; float: right; vertical-align: middle" src="maricultura-team.jpg" alt="Team image">
  <p style="text-align: justify;" class="lead">Our team consists of 5 master’s students at the Bren School of Environmental Science & Management at the University of California, Santa Barbara: Anna Calle, Caio Vianna, Eva Marrero, Kirby Barttlet and Sandra Fogg. We came together as a group with a common interest in marine conservation and ocean sustainability. Our collaborative thesis is in partnership with the World Wildlife Fund in Brazil.</p>
  <p class="lead">For more information visit our website.</p>
  <p style="display: inline-block; vertical-align: bottom" class="lead">
    <a class="btn btn-primary btn-lg" href=https://maricultura.weebly.com" target="_blank" role="button">Learn more</a>
  </p>
  </div>
</div>')

aboutPage <- tabPanel(div(icon("info-circle"),"About"),
                      header,
                      project,
                      creators)
               
               