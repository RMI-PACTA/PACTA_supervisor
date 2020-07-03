if (typeof d3 == "undefined") {
  var imported = document.createElement("script");
  imported.src = "js/d3.v4.min.js";
  document.head.appendChild(imported);
}

class bubble2 {

  constructor(container, data, opts) {
    document.querySelector(container).classList.add("BubbleChart");
    document.querySelector(container).classList.add("d3chart");
    
    this.container = d3.select(container);
    
    opts = (typeof opts === 'undefined') ? {} : opts;
    
    this.namevar = (typeof opts.namevar === "undefined") ? "portfolio_name" : opts.namevar;
    this.xvar = (typeof opts.xvar === "undefined") ? "green" : opts.xvar;
    this.yvar = (typeof opts.yvar === "undefined") ? "brown" : opts.yvar;
    this.zvar = (typeof opts.zvar === "undefined") ? "total" : opts.zvar;
	this.vvar = (typeof opts.vvar === "undefined") ? "total" : opts.vvar;
	this.color_ = (typeof opts.color_ === "undefined") ? "color" : opts.color_;
    this.bkg_fill = (typeof opts.bkg_fill === "undefined") ? false : opts.bkg_fill;
    this.xtitle = (typeof opts.xtitle === "undefined") ? this.xvar : opts.xtitle;
    this.ytitle = (typeof opts.ytitle === "undefined") ? this.yvar : opts.ytitle;
    this.xintcpt = (typeof opts.xintcpt === "undefined") ? false : opts.xintcpt;
    this.yintcpt = (typeof opts.yintcpt === "undefined") ? false : opts.yintcpt;
    this.xticksfrmt = (typeof opts.xticksfrmt === "undefined") ? ".0%" : opts.xticksfrmt;
    this.yticksfrmt = (typeof opts.yticksfrmt === "undefined") ? ".0%" : opts.yticksfrmt;

    this.margin = {top: 20, right: 20, bottom: 100, left: 70};
    this.width = 375 - this.margin.left - this.margin.right;
    this.height = 375 - this.margin.top - this.margin.bottom;
    
    this.svg = this.container
      .append("svg")
      .attr("width", this.width + this.margin.left + this.margin.right)
      .attr("height", this.height + this.margin.top + this.margin.bottom)
      .append("g")
      .attr("transform", "translate(" + this.margin.left + "," + this.margin.top + ")")
    ;

    this.x = d3.scaleLinear().range([0, this.width]);
    this.y = d3.scaleLinear().range([this.height, 0]);
    this.z = d3.scaleLinear().range([1, 20]);

    this.tooltip = this.container
      .append("div")
      .attr("class", "d3tooltip")
      .style("display", "none")
      ;

    this.svg.append("defs")
      .append("linearGradient")
      .attr("id", "linear-gradient")
      .attr("gradientTransform", "rotate(-45,0.5,0.5)")
      .selectAll("stop")
      .data([
        {offset: "0%", color: "#d62728"},
        {offset: "50%", color: "#ffffcc"},
        {offset: "100%", color: "#2ca02c"}
      ])
      .enter().append("stop")
      .attr("offset", d => d.offset)
      .attr("stop-color", d => d.color)
    ;

    this.svg.append("rect")
      .attr("x", 0)
      .attr("y", 0)
      .attr("width", this.width)
      .attr("height", this.height)
      .style("fill", this.bkg_fill ? "url('#linear-gradient')" : "white")
      .style("stroke-width", "1px")
      .style("stroke", this.bkg_fill ? "none" : "black")
      .style("opacity", "0.6")
    ;

    this.svg.append("line")
      .attr("x1", this.x(0.5))
      .attr("x2", this.x(0.5))
      .attr("y1", this.y(0))
      .attr("y2", this.y(1))
      .attr("stroke", this.xintcpt ? "black" : "none")
      .attr("stroke-dasharray", "1, 2")
    ;

    this.svg.append("line")
      .attr("x1", this.x(0))
      .attr("x2", this.x(1))
      .attr("y1", this.y(0.5))
      .attr("y2", this.y(0.5))
      .attr("stroke", this.yintcpt ? "black" : "none")
      .attr("stroke-dasharray", "1, 2")
    ;

    this.update(data);
  }

  update(data) {
    this.data = data;
    var chart = this;

    var buffer = 0.00;    
    this.x.domain([d3.min(this.data, d => d[this.xvar]) - buffer, d3.max(this.data, d => d[this.xvar]) + buffer]).nice();
    this.y.domain([d3.min(this.data, d => d[this.yvar]) - buffer, d3.max(this.data, d => d[this.yvar]) + buffer]).nice();
    

    var xaxis = d3.axisBottom(this.x)
      .ticks(6)
      .tickFormat(d3.format(this.xticksfrmt))
    ;

    var yaxis = d3.axisLeft(this.y)
      .ticks(6)
      .tickFormat(d3.format(this.yticksfrmt))
    ;

    this.svg.append("g")
      .style("stroke-width", "0px")
      .attr("transform", "translate(0," + this.height + ")")
      .call(xaxis)
    ;

    this.svg.append("text")
      .attr("transform", "translate(" + (this.width/2) + " ," +
                          (this.height + this.margin.top + 20) + ")")
      .style("text-anchor", "middle")
      .text(this.xtitle)
    ;

    this.svg.append("g")
      .style("stroke-width", "0px")
      .call(yaxis)
    ;

    this.svg.append("text")
      .attr("transform", "rotate(-90)")
      .attr("y", 0 - this.margin.left)
      .attr("x", 0 - (this.height / 2))
      .attr("dy", "1em")
      .style("text-anchor", "middle")
      .text(this.ytitle)
    ;

    this.svg.append("g")
      .selectAll("dot")
      .data(this.data)
      .enter()
      .append("circle")
      .attr("cx", d => chart.x(d[this.xvar]))
      .attr("cy", d => chart.y(d[this.yvar]))
	  .attr("r", d => d[this.zvar])
      .style("fill", this.color_)
      .style("opacity", "0.7")
      .attr("stroke", "black")
      .on("mouseover", mouseover)
      .on("mousemove", mousemove)
      .on("mouseout", mouseout)
    ;

    function mouseover(d) {
      chart.tooltip
        .html(d[chart.namevar] + "<br>" +
              chart.xvar + ": " + d3.format(".1%")(d[chart.xvar]) + "<br>" +
              chart.yvar + ": " + d3.format(".1%")(d[chart.yvar]) + "<br>" +
              chart.vvar + ": " + d3.format(".1%")(d[chart.vvar]))
        .style("display", "inline-block")
        .style("left", d3.event.pageX + 10 + "px")
        .style("top", d3.event.pageY - 20 + "px")
    }

    function mousemove() {
      chart.tooltip
        .style("left", d3.event.pageX + 10 + "px")
        .style("top", d3.event.pageY - 20 + "px")
    }

    function mouseout() {
      chart.tooltip
        .style("display", "none")
    }
  }
}
