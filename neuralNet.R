# Directions ----
#' 1) Place this file into the same directory as app.R
#' 2) In app.R, after last library call, place the following
#' uncommented out command:
# source("neuralNet.R")
#' 3) In the appropriate section of the UI portion place the following
#'  uncommented command. If you need to change alignment, wrap in a div.
#  neuralNet
#' 4) Immediately after (don't forget the comma), place the following
#'  uncommented out code:
#   tags$script(HTML(
#     "document.getElementById('neuralNet').focus();
#     $('#objects').on('click', '.highlightNode', (ev) => {
#     Shiny.setInputValue('clickedElement', ev.target.id);
#     })"
#   )),
#   tags$script(HTML(
#     "document.getElementById('neuralNet').focus();
#     $('#objects').on('click', '.highlightArrow', (ev) => {
#     Shiny.setInputValue('clickedElement', ev.target.parentNode.parentNode.id);
#     })"
#   )),
#' 5) Where appropriate in the server portion, you may access input$clickedElement
#' to see what the user has clicked on.

# Object Names ----
#' The following are the names of all objects
#' Nodes: input1, input2, input3, node1-1, node1-2, node1-3, node1-4,
#' node2-1, node2-2, node2-3, node2-4, output
#' Arrows: connections1, connections2, connections3

# Neural Net Object Definition ----
# Avoid changing any ofthe following
neuralNet <- HTML('<?xml version="1.0" encoding="UTF-8"?>
<svg id="neuralNet" width="50%" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 1132 545">
  <defs>
    <style>
      .backdrop {
        fill: #fff;
      }

      .labelText {
        font-family: MyriadPro-Regular, "Myriad Pro";
        font-size: 28px;
      }

      .labelText1 {
        letter-spacing: 0em;
      }

      .labelText2 {
        letter-spacing: -.01em;
      }

      .inputNodes, .layer1Nodes, .outputNodes, .layer2Nodes, .arrowLines {
        stroke: #000;
        stroke-miterlimit: 10;
        stroke-width: 4px;
      }

      .inputNodes {
        fill: #1e407c;
      }

      .layer1Nodes {
        fill: #9c0;
      }

      .layer2Nodes {
        fill: #ac8dce;
      }

      .outputNodes {
        fill: #bc204b;
      }

      .highlightNode, .highlightArrow {
        cursor: pointer;
      }

      .highlightNode:hover {
        stroke: yellow;
        stroke-width: 4px;
      }

      .highlightArrow {
        stroke: black;
        stroke-width: 4px;
      }

      .highlightArrow:hover {
        stroke: yellow;
        fill: yellow;
      }
    </style>
  </defs>
  <g id="uuid-384fc45e-94dd-46d8-9973-ac88b58f6002" data-name="background">
    <rect class="backdrop" width="1132" height="545"/>
  </g>
<g id="objects">
  <g data-name="input layer">
    <circle id="input3" class="scene-object inputNodes highlightNode" cx="72" cy="127" r="37.5"/>
    <circle id="input1" class="inputNodes highlightNode" cx="71.5" cy="362.5" r="37.5"/>
    <circle id="input2" class="inputNodes highlightNode" cx="71.5" cy="244.5" r="37.5"/>
    <text class="labelText" transform="translate(10.24 533.71)"><tspan class="labelText1" x="0" y="0">I</tspan><tspan x="6.97" y="0">nput L</tspan><tspan class="labelText1" x="82.29" y="0">a</tspan><tspan class="labelText1" x="95.56" y="0">y</tspan><tspan x="108.47" y="0">er</tspan></text>
  </g>
  <g id="uuid-f1406134-05d1-4786-9f20-a3564d354408" data-name="layer 1">
    <circle id="node1-3" class="layer1Nodes highlightNode" cx="366.89" cy="177.08" r="37.5"/>
    <circle id="node1-1" class="layer1Nodes highlightNode" cx="366.5" cy="411.5" r="37.5"/>
    <circle id="node1-2" class="layer1Nodes highlightNode" cx="366.39" cy="294.58" r="37.5"/>
    <circle id="node1-4" class="layer1Nodes highlightNode" cx="366.5" cy="59.5" r="37.5"/>
    <text class="labelText" transform="translate(278.12 532.78)"><tspan class="labelText1" x="0" y="0">H</tspan><tspan x="18.54" y="0">idden L</tspan><tspan class="labelText1" x="105.39" y="0">a</tspan><tspan class="labelText2" x="118.66" y="0">y</tspan><tspan x="131.57" y="0">er 1</tspan></text>
  </g>
  <g id="uuid-8bfa4a4b-e8bc-4d4c-9cab-9d0f8d27ebeb" data-name="layer 2">
    <circle id="node2-3" class="layer2Nodes highlightNode" cx="728.19" cy="176.86" r="37.5"/>
    <circle id="node2-1" class="layer2Nodes highlightNode" cx="727.8" cy="411.29" r="37.5"/>
    <circle id="node2-2" class="layer2Nodes highlightNode" cx="727.69" cy="294.36" r="37.5"/>
    <circle id="node2-4" class="layer2Nodes highlightNode" cx="727.8" cy="59.29" r="37.5"/>
    <text class="labelText" transform="translate(639.42 533.57)"><tspan class="labelText1" x="0" y="0">H</tspan><tspan x="18.54" y="0">idden L</tspan><tspan class="labelText1" x="105.39" y="0">a</tspan><tspan class="labelText2" x="118.66" y="0">y</tspan><tspan x="131.57" y="0">er 2</tspan></text>
  </g>
  <g id="uuid-a9d4a295-f160-4358-9522-0f10ee068f5c" data-name="output layer">
    <circle id="output" class="outputNodes highlightNode" cx="1043.5" cy="256.5" r="37.5"/>
    <text class="labelText" transform="translate(970.42 533.57)"><tspan class="labelText1" x="0" y="0">O</tspan><tspan x="19.38" y="0">utput L</tspan><tspan class="labelText1" x="103.85" y="0">a</tspan><tspan class="labelText1" x="117.12" y="0">y</tspan><tspan x="130.03" y="0">er</tspan></text>
  </g>
  <g id="connections1" class="highlightArrow">
    <g>
      <line x1="110" y1="127" x2="295.16" y2="69.51"/>
      <polygon points="291.03 82.96 329 59 284.14 60.76 291.03 82.96"/>
    </g>
    <g>
      <line x1="110" y1="127" x2="294.45" y2="169.11"/>
      <polygon points="284.14 178.68 329 177 289.31 156.02 284.14 178.68"/>
    </g>
    <g>
      <line x1="110" y1="127" x2="300.88" y2="273.43"/>
      <polygon points="287.52 277.83 329 295 301.67 259.39 287.52 277.83"/>
    </g>
    <g>
      <line x1="110" y1="127" x2="307.36" y2="382.94"/>
      <polygon points="293.32 383.76 329 411 311.72 369.56 293.32 383.76"/>
    </g>
    <g>
      <line x1="109" y1="244" x2="301.88" y2="81.81"/>
      <polygon points="303.29 95.8 329 59 288.33 78.01 303.29 95.8"/>
    </g>
    <g>
      <line x1="109" y1="362" x2="308.18" y2="87.68"/>
      <polygon points="312.93 100.92 329 59 294.12 87.26 312.93 100.92"/>
    </g>
    <g>
      <line x1="109" y1="244" x2="295.1" y2="187.32"/>
      <polygon points="290.9 200.75 329 177 284.13 178.52 290.9 200.75"/>
    </g>
    <g>
      <line x1="109" y1="362" x2="301.88" y2="199.81"/>
      <polygon points="303.29 213.8 329 177 288.33 196.01 303.29 213.8"/>
    </g>
    <g>
      <line x1="109" y1="244" x2="294.48" y2="287"/>
      <polygon points="284.13 296.53 329 295 289.38 273.89 284.13 296.53"/>
    </g>
    <g>
      <line x1="109" y1="362" x2="295.1" y2="305.32"/>
      <polygon points="290.9 318.75 329 295 284.13 296.52 290.9 318.75"/>
    </g>
    <g>
      <line x1="109" y1="244" x2="300.77" y2="389.57"/>
      <polygon points="287.43 394.04 329 411 301.49 375.53 287.43 394.04"/>
    </g>
    <g>
      <line x1="109" y1="362" x2="294.41" y2="403.3"/>
      <polygon points="284.15 412.92 329 411 289.2 390.23 284.15 412.92"/>
    </g>
  </g>
  <g id="connections2" class="highlightArrow">
    <g>
      <line x1="404" y1="59" x2="654.56" y2="59"/>
      <polygon points="646.64 70.62 690 59 646.64 47.38 646.64 70.62"/>
    </g>
    <g>
      <line x1="404" y1="177" x2="657.24" y2="72.52"/>
      <polygon points="654.35 86.28 690 59 645.48 64.8 654.35 86.28"/>
    </g>
    <g>
      <line x1="404" y1="295" x2="662.67" y2="81.55"/>
      <polygon points="663.95 95.56 690 59 649.16 77.64 663.95 95.56"/>
    </g>
    <g>
      <line x1="404" y1="411" x2="667.65" y2="86.5"/>
      <polygon points="671.67 99.98 690 59 653.64 85.33 671.67 99.98"/>
    </g>
    <g>
      <line x1="404" y1="59" x2="658.22" y2="163.52"/>
      <polygon points="646.48 171.26 691 177 655.31 149.76 646.48 171.26"/>
    </g>
    <g>
      <line x1="404" y1="177" x2="655.56" y2="177"/>
      <polygon points="647.64 188.62 691 177 647.64 165.38 647.64 188.62"/>
    </g>
    <g>
      <line x1="404" y1="295" x2="658.22" y2="190.48"/>
      <polygon points="655.31 204.24 691 177 646.48 182.74 655.31 204.24"/>
    </g>
    <g>
      <line x1="404" y1="411" x2="663.53" y2="199.39"/>
      <polygon points="664.74 213.41 691 177 650.05 195.4 664.74 213.41"/>
    </g>
    <g>
      <line x1="404" y1="59" x2="662.62" y2="271.5"/>
      <polygon points="649.12 275.45 690 294 663.87 257.49 649.12 275.45"/>
    </g>
    <g>
      <line x1="404" y1="177" x2="657.2" y2="280.58"/>
      <polygon points="645.47 288.34 690 294 654.27 266.83 645.47 288.34"/>
    </g>
    <g>
      <line x1="404" y1="295" x2="654.56" y2="294.12"/>
      <polygon points="646.68 305.77 690 294 646.6 282.53 646.68 305.77"/>
    </g>
    <g>
      <line x1="404" y1="411" x2="657.2" y2="307.42"/>
      <polygon points="654.27 321.17 690 294 645.47 299.66 654.27 321.17"/>
    </g>
    <g>
      <line x1="404" y1="59" x2="667.65" y2="383.5"/>
      <polygon points="653.64 384.67 690 411 671.67 370.02 653.64 384.67"/>
    </g>
    <g>
      <line x1="404" y1="177" x2="662.57" y2="388.56"/>
      <polygon points="649.08 392.53 690 411 663.8 374.55 649.08 392.53"/>
    </g>
    <g>
      <line x1="404" y1="295" x2="657.16" y2="397.68"/>
      <polygon points="645.45 405.47 690 411 654.18 383.93 645.45 405.47"/>
    </g>
    <g>
      <line x1="404" y1="411" x2="654.56" y2="411"/>
      <polygon points="646.64 422.62 690 411 646.64 399.38 646.64 422.62"/>
    </g>
  </g>
  <g id="connections3" class="highlightArrow">
    <g>
      <line x1="765" y1="59" x2="978.56" y2="233.57"/>
      <polygon points="965.07 237.55 1006 256 979.78 219.56 965.07 237.55"/>
    </g>
    <g>
      <line x1="766" y1="177" x2="972.34" y2="244.92"/>
      <polygon points="961.18 253.48 1006 256 968.44 231.4 961.18 253.48"/>
    </g>
    <g>
      <line x1="765" y1="294" x2="970.99" y2="261.52"/>
      <polygon points="964.98 274.23 1006 256 961.36 251.27 964.98 274.23"/>
    </g>
    <g>
      <line x1="765" y1="411" x2="976.19" y2="275.17"/>
      <polygon points="975.81 289.23 1006 256 963.24 269.68 975.81 289.23"/>
    </g>
  </g>
</g>
</svg>')