# nmfs_palette() works

    Code
      coral_palette(10)
    Output
       [1] "#FF6C57" "#F3533C" "#E73A21" "#DB2207" "#CF1D04" "#C31702" "#B71300"
       [8] "#AA1200" "#9D1200" "#901200"

---

    Code
      coral_palette_rev(10)
    Output
       [1] "#901200" "#9D1200" "#AA1200" "#B71300" "#C31802" "#CF1D04" "#DB2207"
       [8] "#E63A21" "#F3533C" "#FF6C57"

# display_nmfs_palette() works

    Code
      urchin_palette$data
    Output
        x y   color
      1 1 1 #A8B8FF
      2 1 2 #737BE6
      3 1 3 #5761C0
      4 1 4 #3B469A

# nmfs_theme() works

    Code
      theme_nmfs(palette = "urchin", discrete = FALSE)
    Output
      <theme> List of 146
       $ line                            : <ggplot2::element_line>
        ..@ colour       : chr "black"
        ..@ linewidth    : num 0.636
        ..@ linetype     : num 1
        ..@ lineend      : chr "butt"
        ..@ linejoin     : chr "round"
        ..@ arrow        : logi FALSE
        ..@ arrow.fill   : chr "black"
        ..@ inherit.blank: logi TRUE
       $ rect                            : <ggplot2::element_rect>
        ..@ fill         : chr "white"
        ..@ colour       : chr "black"
        ..@ linewidth    : num 0.636
        ..@ linetype     : num 1
        ..@ linejoin     : chr "round"
        ..@ inherit.blank: logi TRUE
       $ text                            : <ggplot2::element_text>
        ..@ family       : chr ""
        ..@ face         : chr "plain"
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : chr "black"
        ..@ size         : num 14
        ..@ hjust        : num 0.5
        ..@ vjust        : num 0.5
        ..@ angle        : num 0
        ..@ lineheight   : num 0.9
        ..@ margin       : <ggplot2::margin> num [1:4] 0 0 0 0
        ..@ debug        : logi FALSE
        ..@ inherit.blank: logi TRUE
       $ title                           : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : NULL
        ..@ size         : NULL
        ..@ hjust        : NULL
        ..@ vjust        : NULL
        ..@ angle        : NULL
        ..@ lineheight   : NULL
        ..@ margin       : NULL
        ..@ debug        : NULL
        ..@ inherit.blank: logi TRUE
       $ point                           : <ggplot2::element_point>
        ..@ colour       : chr "black"
        ..@ shape        : num 19
        ..@ size         : num 1.91
        ..@ fill         : chr "white"
        ..@ stroke       : num 0.636
        ..@ inherit.blank: logi TRUE
       $ polygon                         : <ggplot2::element_polygon>
        ..@ fill         : chr "white"
        ..@ colour       : chr "black"
        ..@ linewidth    : num 0.636
        ..@ linetype     : num 1
        ..@ linejoin     : chr "round"
        ..@ inherit.blank: logi TRUE
       $ geom                            : <ggplot2::element_geom>
        ..@ ink        : chr "black"
        ..@ paper      : chr "white"
        ..@ accent     : chr "#003087"
        ..@ linewidth  : num 0.636
        ..@ borderwidth: num 0.636
        ..@ linetype   : int 1
        ..@ bordertype : int 1
        ..@ family     : chr ""
        ..@ fontsize   : num 4.92
        ..@ pointsize  : num 1.91
        ..@ pointshape : num 19
        ..@ colour     : NULL
        ..@ fill       : NULL
       $ spacing                         : 'simpleUnit' num 7points
        ..- attr(*, "unit")= int 8
       $ margins                         : <ggplot2::margin> num [1:4] 7 7 7 7
       $ aspect.ratio                    : NULL
       $ axis.title                      : NULL
       $ axis.title.x                    : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : NULL
        ..@ size         : NULL
        ..@ hjust        : NULL
        ..@ vjust        : num 1
        ..@ angle        : NULL
        ..@ lineheight   : NULL
        ..@ margin       : <ggplot2::margin> num [1:4] 3.5 0 0 0
        ..@ debug        : NULL
        ..@ inherit.blank: logi TRUE
       $ axis.title.x.top                : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : NULL
        ..@ size         : NULL
        ..@ hjust        : NULL
        ..@ vjust        : num 0
        ..@ angle        : NULL
        ..@ lineheight   : NULL
        ..@ margin       : <ggplot2::margin> num [1:4] 0 0 3.5 0
        ..@ debug        : NULL
        ..@ inherit.blank: logi TRUE
       $ axis.title.x.bottom             : NULL
       $ axis.title.y                    : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : NULL
        ..@ size         : NULL
        ..@ hjust        : NULL
        ..@ vjust        : num 1
        ..@ angle        : num 90
        ..@ lineheight   : NULL
        ..@ margin       : <ggplot2::margin> num [1:4] 0 3.5 0 0
        ..@ debug        : NULL
        ..@ inherit.blank: logi TRUE
       $ axis.title.y.left               : NULL
       $ axis.title.y.right              : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : NULL
        ..@ size         : NULL
        ..@ hjust        : NULL
        ..@ vjust        : num 1
        ..@ angle        : num -90
        ..@ lineheight   : NULL
        ..@ margin       : <ggplot2::margin> num [1:4] 0 0 0 3.5
        ..@ debug        : NULL
        ..@ inherit.blank: logi TRUE
       $ axis.text                       : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : chr "#4D4D4DFF"
        ..@ size         : 'rel' num 0.8
        ..@ hjust        : NULL
        ..@ vjust        : NULL
        ..@ angle        : NULL
        ..@ lineheight   : NULL
        ..@ margin       : NULL
        ..@ debug        : NULL
        ..@ inherit.blank: logi TRUE
       $ axis.text.x                     : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : chr "black"
        ..@ size         : num 12
        ..@ hjust        : NULL
        ..@ vjust        : num 1
        ..@ angle        : NULL
        ..@ lineheight   : NULL
        ..@ margin       : <ggplot2::margin> num [1:4] 2.8 0 0 0
        ..@ debug        : NULL
        ..@ inherit.blank: logi FALSE
       $ axis.text.x.top                 : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : NULL
        ..@ size         : NULL
        ..@ hjust        : NULL
        ..@ vjust        : num 0
        ..@ angle        : NULL
        ..@ lineheight   : NULL
        ..@ margin       : <ggplot2::margin> num [1:4] 0 0 2.8 0
        ..@ debug        : NULL
        ..@ inherit.blank: logi TRUE
       $ axis.text.x.bottom              : NULL
       $ axis.text.y                     : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : chr "black"
        ..@ size         : num 12
        ..@ hjust        : num 1
        ..@ vjust        : NULL
        ..@ angle        : NULL
        ..@ lineheight   : NULL
        ..@ margin       : <ggplot2::margin> num [1:4] 0 2.8 0 0
        ..@ debug        : NULL
        ..@ inherit.blank: logi FALSE
       $ axis.text.y.left                : NULL
       $ axis.text.y.right               : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : NULL
        ..@ size         : NULL
        ..@ hjust        : num 0
        ..@ vjust        : NULL
        ..@ angle        : NULL
        ..@ lineheight   : NULL
        ..@ margin       : <ggplot2::margin> num [1:4] 0 0 0 2.8
        ..@ debug        : NULL
        ..@ inherit.blank: logi TRUE
       $ axis.text.theta                 : NULL
       $ axis.text.r                     : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : NULL
        ..@ size         : NULL
        ..@ hjust        : num 0.5
        ..@ vjust        : NULL
        ..@ angle        : NULL
        ..@ lineheight   : NULL
        ..@ margin       : <ggplot2::margin> num [1:4] 0 2.8 0 2.8
        ..@ debug        : NULL
        ..@ inherit.blank: logi TRUE
       $ axis.ticks                      : <ggplot2::element_line>
        ..@ colour       : chr "#333333FF"
        ..@ linewidth    : NULL
        ..@ linetype     : NULL
        ..@ lineend      : NULL
        ..@ linejoin     : NULL
        ..@ arrow        : logi FALSE
        ..@ arrow.fill   : chr "#333333FF"
        ..@ inherit.blank: logi TRUE
       $ axis.ticks.x                    : NULL
       $ axis.ticks.x.top                : NULL
       $ axis.ticks.x.bottom             : NULL
       $ axis.ticks.y                    : NULL
       $ axis.ticks.y.left               : NULL
       $ axis.ticks.y.right              : NULL
       $ axis.ticks.theta                : NULL
       $ axis.ticks.r                    : NULL
       $ axis.minor.ticks.x.top          : NULL
       $ axis.minor.ticks.x.bottom       : NULL
       $ axis.minor.ticks.y.left         : NULL
       $ axis.minor.ticks.y.right        : NULL
       $ axis.minor.ticks.theta          : NULL
       $ axis.minor.ticks.r              : NULL
       $ axis.ticks.length               : 'rel' num 0.5
       $ axis.ticks.length.x             : NULL
       $ axis.ticks.length.x.top         : NULL
       $ axis.ticks.length.x.bottom      : NULL
       $ axis.ticks.length.y             : NULL
       $ axis.ticks.length.y.left        : NULL
       $ axis.ticks.length.y.right       : NULL
       $ axis.ticks.length.theta         : NULL
       $ axis.ticks.length.r             : NULL
       $ axis.minor.ticks.length         : 'rel' num 0.75
       $ axis.minor.ticks.length.x       : NULL
       $ axis.minor.ticks.length.x.top   : NULL
       $ axis.minor.ticks.length.x.bottom: NULL
       $ axis.minor.ticks.length.y       : NULL
       $ axis.minor.ticks.length.y.left  : NULL
       $ axis.minor.ticks.length.y.right : NULL
       $ axis.minor.ticks.length.theta   : NULL
       $ axis.minor.ticks.length.r       : NULL
       $ axis.line                       : <ggplot2::element_line>
        ..@ colour       : chr "black"
        ..@ linewidth    : num 0.75
        ..@ linetype     : NULL
        ..@ lineend      : NULL
        ..@ linejoin     : NULL
        ..@ arrow        : logi FALSE
        ..@ arrow.fill   : chr "black"
        ..@ inherit.blank: logi FALSE
       $ axis.line.x                     : NULL
       $ axis.line.x.top                 : NULL
       $ axis.line.x.bottom              : NULL
       $ axis.line.y                     : NULL
       $ axis.line.y.left                : NULL
       $ axis.line.y.right               : NULL
       $ axis.line.theta                 : NULL
       $ axis.line.r                     : NULL
       $ legend.background               : <ggplot2::element_rect>
        ..@ fill         : NULL
        ..@ colour       : logi NA
        ..@ linewidth    : NULL
        ..@ linetype     : NULL
        ..@ linejoin     : NULL
        ..@ inherit.blank: logi TRUE
       $ legend.margin                   : NULL
       $ legend.spacing                  : 'rel' num 2
       $ legend.spacing.x                : NULL
       $ legend.spacing.y                : NULL
       $ legend.key                      : NULL
       $ legend.key.size                 : 'simpleUnit' num 1.2lines
        ..- attr(*, "unit")= int 3
       $ legend.key.height               : NULL
       $ legend.key.width                : NULL
       $ legend.key.spacing              : NULL
       $ legend.key.spacing.x            : NULL
       $ legend.key.spacing.y            : NULL
       $ legend.key.justification        : NULL
       $ legend.frame                    : NULL
       $ legend.ticks                    : NULL
       $ legend.ticks.length             : 'rel' num 0.2
       $ legend.axis.line                : NULL
       $ legend.text                     : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : NULL
        ..@ size         : 'rel' num 0.8
        ..@ hjust        : NULL
        ..@ vjust        : NULL
        ..@ angle        : NULL
        ..@ lineheight   : NULL
        ..@ margin       : NULL
        ..@ debug        : NULL
        ..@ inherit.blank: logi TRUE
       $ legend.text.position            : NULL
       $ legend.title                    : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : NULL
        ..@ size         : NULL
        ..@ hjust        : num 0
        ..@ vjust        : NULL
        ..@ angle        : NULL
        ..@ lineheight   : NULL
        ..@ margin       : NULL
        ..@ debug        : NULL
        ..@ inherit.blank: logi TRUE
       $ legend.title.position           : NULL
       $ legend.position                 : chr "right"
       $ legend.position.inside          : NULL
       $ legend.direction                : NULL
       $ legend.byrow                    : NULL
       $ legend.justification            : chr "center"
       $ legend.justification.top        : NULL
       $ legend.justification.bottom     : NULL
       $ legend.justification.left       : NULL
       $ legend.justification.right      : NULL
       $ legend.justification.inside     : NULL
        [list output truncated]
       @ complete: logi TRUE
       @ validate: logi TRUE

---

    Code
      theme_nmfs(palette = "crustacean", discrete = TRUE, interpolate = TRUE)
    Output
      <theme> List of 146
       $ line                            : <ggplot2::element_line>
        ..@ colour       : chr "black"
        ..@ linewidth    : num 0.636
        ..@ linetype     : num 1
        ..@ lineend      : chr "butt"
        ..@ linejoin     : chr "round"
        ..@ arrow        : logi FALSE
        ..@ arrow.fill   : chr "black"
        ..@ inherit.blank: logi TRUE
       $ rect                            : <ggplot2::element_rect>
        ..@ fill         : chr "white"
        ..@ colour       : chr "black"
        ..@ linewidth    : num 0.636
        ..@ linetype     : num 1
        ..@ linejoin     : chr "round"
        ..@ inherit.blank: logi TRUE
       $ text                            : <ggplot2::element_text>
        ..@ family       : chr ""
        ..@ face         : chr "plain"
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : chr "black"
        ..@ size         : num 14
        ..@ hjust        : num 0.5
        ..@ vjust        : num 0.5
        ..@ angle        : num 0
        ..@ lineheight   : num 0.9
        ..@ margin       : <ggplot2::margin> num [1:4] 0 0 0 0
        ..@ debug        : logi FALSE
        ..@ inherit.blank: logi TRUE
       $ title                           : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : NULL
        ..@ size         : NULL
        ..@ hjust        : NULL
        ..@ vjust        : NULL
        ..@ angle        : NULL
        ..@ lineheight   : NULL
        ..@ margin       : NULL
        ..@ debug        : NULL
        ..@ inherit.blank: logi TRUE
       $ point                           : <ggplot2::element_point>
        ..@ colour       : chr "black"
        ..@ shape        : num 19
        ..@ size         : num 1.91
        ..@ fill         : chr "white"
        ..@ stroke       : num 0.636
        ..@ inherit.blank: logi TRUE
       $ polygon                         : <ggplot2::element_polygon>
        ..@ fill         : chr "white"
        ..@ colour       : chr "black"
        ..@ linewidth    : num 0.636
        ..@ linetype     : num 1
        ..@ linejoin     : chr "round"
        ..@ inherit.blank: logi TRUE
       $ geom                            : <ggplot2::element_geom>
        ..@ ink        : chr "black"
        ..@ paper      : chr "white"
        ..@ accent     : chr "#003087"
        ..@ linewidth  : num 0.636
        ..@ borderwidth: num 0.636
        ..@ linetype   : int 1
        ..@ bordertype : int 1
        ..@ family     : chr ""
        ..@ fontsize   : num 4.92
        ..@ pointsize  : num 1.91
        ..@ pointshape : num 19
        ..@ colour     : NULL
        ..@ fill       : NULL
       $ spacing                         : 'simpleUnit' num 7points
        ..- attr(*, "unit")= int 8
       $ margins                         : <ggplot2::margin> num [1:4] 7 7 7 7
       $ aspect.ratio                    : NULL
       $ axis.title                      : NULL
       $ axis.title.x                    : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : NULL
        ..@ size         : NULL
        ..@ hjust        : NULL
        ..@ vjust        : num 1
        ..@ angle        : NULL
        ..@ lineheight   : NULL
        ..@ margin       : <ggplot2::margin> num [1:4] 3.5 0 0 0
        ..@ debug        : NULL
        ..@ inherit.blank: logi TRUE
       $ axis.title.x.top                : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : NULL
        ..@ size         : NULL
        ..@ hjust        : NULL
        ..@ vjust        : num 0
        ..@ angle        : NULL
        ..@ lineheight   : NULL
        ..@ margin       : <ggplot2::margin> num [1:4] 0 0 3.5 0
        ..@ debug        : NULL
        ..@ inherit.blank: logi TRUE
       $ axis.title.x.bottom             : NULL
       $ axis.title.y                    : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : NULL
        ..@ size         : NULL
        ..@ hjust        : NULL
        ..@ vjust        : num 1
        ..@ angle        : num 90
        ..@ lineheight   : NULL
        ..@ margin       : <ggplot2::margin> num [1:4] 0 3.5 0 0
        ..@ debug        : NULL
        ..@ inherit.blank: logi TRUE
       $ axis.title.y.left               : NULL
       $ axis.title.y.right              : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : NULL
        ..@ size         : NULL
        ..@ hjust        : NULL
        ..@ vjust        : num 1
        ..@ angle        : num -90
        ..@ lineheight   : NULL
        ..@ margin       : <ggplot2::margin> num [1:4] 0 0 0 3.5
        ..@ debug        : NULL
        ..@ inherit.blank: logi TRUE
       $ axis.text                       : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : chr "#4D4D4DFF"
        ..@ size         : 'rel' num 0.8
        ..@ hjust        : NULL
        ..@ vjust        : NULL
        ..@ angle        : NULL
        ..@ lineheight   : NULL
        ..@ margin       : NULL
        ..@ debug        : NULL
        ..@ inherit.blank: logi TRUE
       $ axis.text.x                     : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : chr "black"
        ..@ size         : num 12
        ..@ hjust        : NULL
        ..@ vjust        : num 1
        ..@ angle        : NULL
        ..@ lineheight   : NULL
        ..@ margin       : <ggplot2::margin> num [1:4] 2.8 0 0 0
        ..@ debug        : NULL
        ..@ inherit.blank: logi FALSE
       $ axis.text.x.top                 : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : NULL
        ..@ size         : NULL
        ..@ hjust        : NULL
        ..@ vjust        : num 0
        ..@ angle        : NULL
        ..@ lineheight   : NULL
        ..@ margin       : <ggplot2::margin> num [1:4] 0 0 2.8 0
        ..@ debug        : NULL
        ..@ inherit.blank: logi TRUE
       $ axis.text.x.bottom              : NULL
       $ axis.text.y                     : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : chr "black"
        ..@ size         : num 12
        ..@ hjust        : num 1
        ..@ vjust        : NULL
        ..@ angle        : NULL
        ..@ lineheight   : NULL
        ..@ margin       : <ggplot2::margin> num [1:4] 0 2.8 0 0
        ..@ debug        : NULL
        ..@ inherit.blank: logi FALSE
       $ axis.text.y.left                : NULL
       $ axis.text.y.right               : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : NULL
        ..@ size         : NULL
        ..@ hjust        : num 0
        ..@ vjust        : NULL
        ..@ angle        : NULL
        ..@ lineheight   : NULL
        ..@ margin       : <ggplot2::margin> num [1:4] 0 0 0 2.8
        ..@ debug        : NULL
        ..@ inherit.blank: logi TRUE
       $ axis.text.theta                 : NULL
       $ axis.text.r                     : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : NULL
        ..@ size         : NULL
        ..@ hjust        : num 0.5
        ..@ vjust        : NULL
        ..@ angle        : NULL
        ..@ lineheight   : NULL
        ..@ margin       : <ggplot2::margin> num [1:4] 0 2.8 0 2.8
        ..@ debug        : NULL
        ..@ inherit.blank: logi TRUE
       $ axis.ticks                      : <ggplot2::element_line>
        ..@ colour       : chr "#333333FF"
        ..@ linewidth    : NULL
        ..@ linetype     : NULL
        ..@ lineend      : NULL
        ..@ linejoin     : NULL
        ..@ arrow        : logi FALSE
        ..@ arrow.fill   : chr "#333333FF"
        ..@ inherit.blank: logi TRUE
       $ axis.ticks.x                    : NULL
       $ axis.ticks.x.top                : NULL
       $ axis.ticks.x.bottom             : NULL
       $ axis.ticks.y                    : NULL
       $ axis.ticks.y.left               : NULL
       $ axis.ticks.y.right              : NULL
       $ axis.ticks.theta                : NULL
       $ axis.ticks.r                    : NULL
       $ axis.minor.ticks.x.top          : NULL
       $ axis.minor.ticks.x.bottom       : NULL
       $ axis.minor.ticks.y.left         : NULL
       $ axis.minor.ticks.y.right        : NULL
       $ axis.minor.ticks.theta          : NULL
       $ axis.minor.ticks.r              : NULL
       $ axis.ticks.length               : 'rel' num 0.5
       $ axis.ticks.length.x             : NULL
       $ axis.ticks.length.x.top         : NULL
       $ axis.ticks.length.x.bottom      : NULL
       $ axis.ticks.length.y             : NULL
       $ axis.ticks.length.y.left        : NULL
       $ axis.ticks.length.y.right       : NULL
       $ axis.ticks.length.theta         : NULL
       $ axis.ticks.length.r             : NULL
       $ axis.minor.ticks.length         : 'rel' num 0.75
       $ axis.minor.ticks.length.x       : NULL
       $ axis.minor.ticks.length.x.top   : NULL
       $ axis.minor.ticks.length.x.bottom: NULL
       $ axis.minor.ticks.length.y       : NULL
       $ axis.minor.ticks.length.y.left  : NULL
       $ axis.minor.ticks.length.y.right : NULL
       $ axis.minor.ticks.length.theta   : NULL
       $ axis.minor.ticks.length.r       : NULL
       $ axis.line                       : <ggplot2::element_line>
        ..@ colour       : chr "black"
        ..@ linewidth    : num 0.75
        ..@ linetype     : NULL
        ..@ lineend      : NULL
        ..@ linejoin     : NULL
        ..@ arrow        : logi FALSE
        ..@ arrow.fill   : chr "black"
        ..@ inherit.blank: logi FALSE
       $ axis.line.x                     : NULL
       $ axis.line.x.top                 : NULL
       $ axis.line.x.bottom              : NULL
       $ axis.line.y                     : NULL
       $ axis.line.y.left                : NULL
       $ axis.line.y.right               : NULL
       $ axis.line.theta                 : NULL
       $ axis.line.r                     : NULL
       $ legend.background               : <ggplot2::element_rect>
        ..@ fill         : NULL
        ..@ colour       : logi NA
        ..@ linewidth    : NULL
        ..@ linetype     : NULL
        ..@ linejoin     : NULL
        ..@ inherit.blank: logi TRUE
       $ legend.margin                   : NULL
       $ legend.spacing                  : 'rel' num 2
       $ legend.spacing.x                : NULL
       $ legend.spacing.y                : NULL
       $ legend.key                      : NULL
       $ legend.key.size                 : 'simpleUnit' num 1.2lines
        ..- attr(*, "unit")= int 3
       $ legend.key.height               : NULL
       $ legend.key.width                : NULL
       $ legend.key.spacing              : NULL
       $ legend.key.spacing.x            : NULL
       $ legend.key.spacing.y            : NULL
       $ legend.key.justification        : NULL
       $ legend.frame                    : NULL
       $ legend.ticks                    : NULL
       $ legend.ticks.length             : 'rel' num 0.2
       $ legend.axis.line                : NULL
       $ legend.text                     : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : NULL
        ..@ size         : 'rel' num 0.8
        ..@ hjust        : NULL
        ..@ vjust        : NULL
        ..@ angle        : NULL
        ..@ lineheight   : NULL
        ..@ margin       : NULL
        ..@ debug        : NULL
        ..@ inherit.blank: logi TRUE
       $ legend.text.position            : NULL
       $ legend.title                    : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : NULL
        ..@ size         : NULL
        ..@ hjust        : num 0
        ..@ vjust        : NULL
        ..@ angle        : NULL
        ..@ lineheight   : NULL
        ..@ margin       : NULL
        ..@ debug        : NULL
        ..@ inherit.blank: logi TRUE
       $ legend.title.position           : NULL
       $ legend.position                 : chr "right"
       $ legend.position.inside          : NULL
       $ legend.direction                : NULL
       $ legend.byrow                    : NULL
       $ legend.justification            : chr "center"
       $ legend.justification.top        : NULL
       $ legend.justification.bottom     : NULL
       $ legend.justification.left       : NULL
       $ legend.justification.right      : NULL
       $ legend.justification.inside     : NULL
        [list output truncated]
       @ complete: logi TRUE
       @ validate: logi TRUE

---

    Code
      theme_nmfs(palette = "oceans", discrete = TRUE, interpolate = FALSE)
    Message <cliMessage>
      i The oceans palette has 6 colors.
    Output
      <theme> List of 146
       $ line                            : <ggplot2::element_line>
        ..@ colour       : chr "black"
        ..@ linewidth    : num 0.636
        ..@ linetype     : num 1
        ..@ lineend      : chr "butt"
        ..@ linejoin     : chr "round"
        ..@ arrow        : logi FALSE
        ..@ arrow.fill   : chr "black"
        ..@ inherit.blank: logi TRUE
       $ rect                            : <ggplot2::element_rect>
        ..@ fill         : chr "white"
        ..@ colour       : chr "black"
        ..@ linewidth    : num 0.636
        ..@ linetype     : num 1
        ..@ linejoin     : chr "round"
        ..@ inherit.blank: logi TRUE
       $ text                            : <ggplot2::element_text>
        ..@ family       : chr ""
        ..@ face         : chr "plain"
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : chr "black"
        ..@ size         : num 14
        ..@ hjust        : num 0.5
        ..@ vjust        : num 0.5
        ..@ angle        : num 0
        ..@ lineheight   : num 0.9
        ..@ margin       : <ggplot2::margin> num [1:4] 0 0 0 0
        ..@ debug        : logi FALSE
        ..@ inherit.blank: logi TRUE
       $ title                           : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : NULL
        ..@ size         : NULL
        ..@ hjust        : NULL
        ..@ vjust        : NULL
        ..@ angle        : NULL
        ..@ lineheight   : NULL
        ..@ margin       : NULL
        ..@ debug        : NULL
        ..@ inherit.blank: logi TRUE
       $ point                           : <ggplot2::element_point>
        ..@ colour       : chr "black"
        ..@ shape        : num 19
        ..@ size         : num 1.91
        ..@ fill         : chr "white"
        ..@ stroke       : num 0.636
        ..@ inherit.blank: logi TRUE
       $ polygon                         : <ggplot2::element_polygon>
        ..@ fill         : chr "white"
        ..@ colour       : chr "black"
        ..@ linewidth    : num 0.636
        ..@ linetype     : num 1
        ..@ linejoin     : chr "round"
        ..@ inherit.blank: logi TRUE
       $ geom                            : <ggplot2::element_geom>
        ..@ ink        : chr "black"
        ..@ paper      : chr "white"
        ..@ accent     : chr "#003087"
        ..@ linewidth  : num 0.636
        ..@ borderwidth: num 0.636
        ..@ linetype   : int 1
        ..@ bordertype : int 1
        ..@ family     : chr ""
        ..@ fontsize   : num 4.92
        ..@ pointsize  : num 1.91
        ..@ pointshape : num 19
        ..@ colour     : NULL
        ..@ fill       : NULL
       $ spacing                         : 'simpleUnit' num 7points
        ..- attr(*, "unit")= int 8
       $ margins                         : <ggplot2::margin> num [1:4] 7 7 7 7
       $ aspect.ratio                    : NULL
       $ axis.title                      : NULL
       $ axis.title.x                    : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : NULL
        ..@ size         : NULL
        ..@ hjust        : NULL
        ..@ vjust        : num 1
        ..@ angle        : NULL
        ..@ lineheight   : NULL
        ..@ margin       : <ggplot2::margin> num [1:4] 3.5 0 0 0
        ..@ debug        : NULL
        ..@ inherit.blank: logi TRUE
       $ axis.title.x.top                : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : NULL
        ..@ size         : NULL
        ..@ hjust        : NULL
        ..@ vjust        : num 0
        ..@ angle        : NULL
        ..@ lineheight   : NULL
        ..@ margin       : <ggplot2::margin> num [1:4] 0 0 3.5 0
        ..@ debug        : NULL
        ..@ inherit.blank: logi TRUE
       $ axis.title.x.bottom             : NULL
       $ axis.title.y                    : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : NULL
        ..@ size         : NULL
        ..@ hjust        : NULL
        ..@ vjust        : num 1
        ..@ angle        : num 90
        ..@ lineheight   : NULL
        ..@ margin       : <ggplot2::margin> num [1:4] 0 3.5 0 0
        ..@ debug        : NULL
        ..@ inherit.blank: logi TRUE
       $ axis.title.y.left               : NULL
       $ axis.title.y.right              : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : NULL
        ..@ size         : NULL
        ..@ hjust        : NULL
        ..@ vjust        : num 1
        ..@ angle        : num -90
        ..@ lineheight   : NULL
        ..@ margin       : <ggplot2::margin> num [1:4] 0 0 0 3.5
        ..@ debug        : NULL
        ..@ inherit.blank: logi TRUE
       $ axis.text                       : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : chr "#4D4D4DFF"
        ..@ size         : 'rel' num 0.8
        ..@ hjust        : NULL
        ..@ vjust        : NULL
        ..@ angle        : NULL
        ..@ lineheight   : NULL
        ..@ margin       : NULL
        ..@ debug        : NULL
        ..@ inherit.blank: logi TRUE
       $ axis.text.x                     : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : chr "black"
        ..@ size         : num 12
        ..@ hjust        : NULL
        ..@ vjust        : num 1
        ..@ angle        : NULL
        ..@ lineheight   : NULL
        ..@ margin       : <ggplot2::margin> num [1:4] 2.8 0 0 0
        ..@ debug        : NULL
        ..@ inherit.blank: logi FALSE
       $ axis.text.x.top                 : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : NULL
        ..@ size         : NULL
        ..@ hjust        : NULL
        ..@ vjust        : num 0
        ..@ angle        : NULL
        ..@ lineheight   : NULL
        ..@ margin       : <ggplot2::margin> num [1:4] 0 0 2.8 0
        ..@ debug        : NULL
        ..@ inherit.blank: logi TRUE
       $ axis.text.x.bottom              : NULL
       $ axis.text.y                     : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : chr "black"
        ..@ size         : num 12
        ..@ hjust        : num 1
        ..@ vjust        : NULL
        ..@ angle        : NULL
        ..@ lineheight   : NULL
        ..@ margin       : <ggplot2::margin> num [1:4] 0 2.8 0 0
        ..@ debug        : NULL
        ..@ inherit.blank: logi FALSE
       $ axis.text.y.left                : NULL
       $ axis.text.y.right               : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : NULL
        ..@ size         : NULL
        ..@ hjust        : num 0
        ..@ vjust        : NULL
        ..@ angle        : NULL
        ..@ lineheight   : NULL
        ..@ margin       : <ggplot2::margin> num [1:4] 0 0 0 2.8
        ..@ debug        : NULL
        ..@ inherit.blank: logi TRUE
       $ axis.text.theta                 : NULL
       $ axis.text.r                     : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : NULL
        ..@ size         : NULL
        ..@ hjust        : num 0.5
        ..@ vjust        : NULL
        ..@ angle        : NULL
        ..@ lineheight   : NULL
        ..@ margin       : <ggplot2::margin> num [1:4] 0 2.8 0 2.8
        ..@ debug        : NULL
        ..@ inherit.blank: logi TRUE
       $ axis.ticks                      : <ggplot2::element_line>
        ..@ colour       : chr "#333333FF"
        ..@ linewidth    : NULL
        ..@ linetype     : NULL
        ..@ lineend      : NULL
        ..@ linejoin     : NULL
        ..@ arrow        : logi FALSE
        ..@ arrow.fill   : chr "#333333FF"
        ..@ inherit.blank: logi TRUE
       $ axis.ticks.x                    : NULL
       $ axis.ticks.x.top                : NULL
       $ axis.ticks.x.bottom             : NULL
       $ axis.ticks.y                    : NULL
       $ axis.ticks.y.left               : NULL
       $ axis.ticks.y.right              : NULL
       $ axis.ticks.theta                : NULL
       $ axis.ticks.r                    : NULL
       $ axis.minor.ticks.x.top          : NULL
       $ axis.minor.ticks.x.bottom       : NULL
       $ axis.minor.ticks.y.left         : NULL
       $ axis.minor.ticks.y.right        : NULL
       $ axis.minor.ticks.theta          : NULL
       $ axis.minor.ticks.r              : NULL
       $ axis.ticks.length               : 'rel' num 0.5
       $ axis.ticks.length.x             : NULL
       $ axis.ticks.length.x.top         : NULL
       $ axis.ticks.length.x.bottom      : NULL
       $ axis.ticks.length.y             : NULL
       $ axis.ticks.length.y.left        : NULL
       $ axis.ticks.length.y.right       : NULL
       $ axis.ticks.length.theta         : NULL
       $ axis.ticks.length.r             : NULL
       $ axis.minor.ticks.length         : 'rel' num 0.75
       $ axis.minor.ticks.length.x       : NULL
       $ axis.minor.ticks.length.x.top   : NULL
       $ axis.minor.ticks.length.x.bottom: NULL
       $ axis.minor.ticks.length.y       : NULL
       $ axis.minor.ticks.length.y.left  : NULL
       $ axis.minor.ticks.length.y.right : NULL
       $ axis.minor.ticks.length.theta   : NULL
       $ axis.minor.ticks.length.r       : NULL
       $ axis.line                       : <ggplot2::element_line>
        ..@ colour       : chr "black"
        ..@ linewidth    : num 0.75
        ..@ linetype     : NULL
        ..@ lineend      : NULL
        ..@ linejoin     : NULL
        ..@ arrow        : logi FALSE
        ..@ arrow.fill   : chr "black"
        ..@ inherit.blank: logi FALSE
       $ axis.line.x                     : NULL
       $ axis.line.x.top                 : NULL
       $ axis.line.x.bottom              : NULL
       $ axis.line.y                     : NULL
       $ axis.line.y.left                : NULL
       $ axis.line.y.right               : NULL
       $ axis.line.theta                 : NULL
       $ axis.line.r                     : NULL
       $ legend.background               : <ggplot2::element_rect>
        ..@ fill         : NULL
        ..@ colour       : logi NA
        ..@ linewidth    : NULL
        ..@ linetype     : NULL
        ..@ linejoin     : NULL
        ..@ inherit.blank: logi TRUE
       $ legend.margin                   : NULL
       $ legend.spacing                  : 'rel' num 2
       $ legend.spacing.x                : NULL
       $ legend.spacing.y                : NULL
       $ legend.key                      : NULL
       $ legend.key.size                 : 'simpleUnit' num 1.2lines
        ..- attr(*, "unit")= int 3
       $ legend.key.height               : NULL
       $ legend.key.width                : NULL
       $ legend.key.spacing              : NULL
       $ legend.key.spacing.x            : NULL
       $ legend.key.spacing.y            : NULL
       $ legend.key.justification        : NULL
       $ legend.frame                    : NULL
       $ legend.ticks                    : NULL
       $ legend.ticks.length             : 'rel' num 0.2
       $ legend.axis.line                : NULL
       $ legend.text                     : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : NULL
        ..@ size         : 'rel' num 0.8
        ..@ hjust        : NULL
        ..@ vjust        : NULL
        ..@ angle        : NULL
        ..@ lineheight   : NULL
        ..@ margin       : NULL
        ..@ debug        : NULL
        ..@ inherit.blank: logi TRUE
       $ legend.text.position            : NULL
       $ legend.title                    : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : NULL
        ..@ size         : NULL
        ..@ hjust        : num 0
        ..@ vjust        : NULL
        ..@ angle        : NULL
        ..@ lineheight   : NULL
        ..@ margin       : NULL
        ..@ debug        : NULL
        ..@ inherit.blank: logi TRUE
       $ legend.title.position           : NULL
       $ legend.position                 : chr "right"
       $ legend.position.inside          : NULL
       $ legend.direction                : NULL
       $ legend.byrow                    : NULL
       $ legend.justification            : chr "center"
       $ legend.justification.top        : NULL
       $ legend.justification.bottom     : NULL
       $ legend.justification.left       : NULL
       $ legend.justification.right      : NULL
       $ legend.justification.inside     : NULL
        [list output truncated]
       @ complete: logi TRUE
       @ validate: logi TRUE

