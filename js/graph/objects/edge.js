/**
 * Constructs an Edge.
 * @param {Node} source This Edge's source Node.
 * @param {Node} target This Edge's target Node.
 * @param {string} id The id of the SVG path element that this Edge represents.
 * @constructor
 */
function Edge(source, target, id) {
    'use strict';

    this.source = source;
    this.target = target;
    this.id = id;
    this.status = 'inactive';
}


/**
 * Updates this Edge's status.
 */
Edge.prototype.updateStatus = function () {
    'use strict';
    
    var oldStatus = this.status;
    
    if (!this.source.isSelected()) {
        this.status = 'inactive';
    } else if (!this.target.isSelected()) {
        this.status = 'takeable';
    } else {
        this.status = 'active';
    }
    
    this.updateSVG(oldStatus);
};


/**
 * Updates the corresponding SVG path element.
 */
Edge.prototype.updateSVG = function (oldStatus) {
    'use strict';
    var path = document.querySelector('#' + this.id);
    var pathSelector = $('#' + this.id);
    var totalLength = path.getTotalLength();
    console.log(typeof(oldStatus) + " :: PATH");
    var newStatus = this.status;
    
    if ((oldStatus == 'inactive' && newStatus == 'takeable') || (oldStatus == 'takeable' && this.status == 'active')) {
        d3.select('#' + this.id)
            .attr("stroke-dasharray", totalLength + " " + totalLength)
            .attr("stroke-dashoffset", totalLength)
            .transition()
            .duration(500)
            .ease("linear")
            .attr("stroke-dashoffset", 0); 
        pathSelector.attr('data-active', newStatus);
    } else {
        $('#' + this.id).attr('data-active', this.status);
    }
};
