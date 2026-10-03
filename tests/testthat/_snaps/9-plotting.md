# ggplot checks without snapshotting the plot

    Code
      p_grid
    Output
      $data
          id time     facet       effect
      1    A 1960 CFESIS: x  0.011129459
      2    A 1961 CFESIS: x  0.011129459
      3    A 1962 CFESIS: x  0.011129459
      4    A 1963 CFESIS: x  0.011129459
      5    A 1964 CFESIS: x  0.011129459
      6    A 1965 CFESIS: x  0.011129459
      7    A 1966 CFESIS: x  0.011129459
      8    A 1967 CFESIS: x  0.011129459
      9    A 1968 CFESIS: x  0.011129459
      10   A 1969 CFESIS: x  0.011129459
      11   A 1970 CFESIS: x  0.011129459
      12   A 1971 CFESIS: x  0.011129459
      13   A 1972 CFESIS: x  0.011129459
      14   A 1973 CFESIS: x  0.011129459
      15   B 1973 CFESIS: x  0.041526061
      16   A 1974 CFESIS: x  0.011129459
      17   B 1974 CFESIS: x  0.041526061
      18   A 1975 CFESIS: x  0.011129459
      19   B 1975 CFESIS: x  0.041526061
      20   A 1976 CFESIS: x  0.011129459
      21   B 1976 CFESIS: x  0.041526061
      22   A 1977 CFESIS: x  0.011129459
      23   B 1977 CFESIS: x  0.041526061
      24   A 1978 CFESIS: x  0.004204517
      25   B 1978 CFESIS: x  0.041526061
      26   A 1979 CFESIS: x  0.004204517
      27   B 1979 CFESIS: x  0.041526061
      28   A 1980 CFESIS: x  0.004204517
      29   B 1980 CFESIS: x  0.041526061
      30   A 1981 CFESIS: x  0.004204517
      31   B 1981 CFESIS: x  0.041526061
      32   A 1982 CFESIS: x  0.004204517
      33   B 1982 CFESIS: x  0.041526061
      34   A 1983 CFESIS: x  0.004204517
      35   B 1983 CFESIS: x  0.041526061
      36   A 1984 CFESIS: x  0.004204517
      37   B 1984 CFESIS: x  0.041526061
      38   A 1985 CFESIS: x  0.004204517
      39   B 1985 CFESIS: x  0.083814116
      40   A 1986 CFESIS: x  0.004204517
      41   B 1986 CFESIS: x  0.083814116
      42   A 1987 CFESIS: x  0.004204517
      43   B 1987 CFESIS: x  0.083814116
      44   A 1988 CFESIS: x -0.009819537
      45   B 1988 CFESIS: x  0.083814116
      46   A 1989 CFESIS: x -0.009819537
      47   B 1989 CFESIS: x  0.083814116
      48   A 1990 CFESIS: x -0.009819537
      49   B 1990 CFESIS: x  0.083814116
      50   A 1991 CFESIS: x -0.009819537
      51   B 1991 CFESIS: x  0.083814116
      52   A 1992 CFESIS: x -0.009819537
      53   B 1992 CFESIS: x  0.083814116
      54   A 1993 CFESIS: x -0.009819537
      55   B 1993 CFESIS: x  0.083814116
      56   A 1994 CFESIS: x -0.009819537
      57   B 1994 CFESIS: x  0.083814116
      58   A 1995 CFESIS: x -0.009819537
      59   B 1995 CFESIS: x  0.083814116
      60   A 1996 CFESIS: x -0.009819537
      61   B 1996 CFESIS: x  0.083814116
      62   A 1997 CFESIS: x -0.009819537
      63   B 1997 CFESIS: x  0.083814116
      64   A 1998 CFESIS: x -0.009819537
      65   B 1998 CFESIS: x  0.083814116
      66   A 1999 CFESIS: x -0.009819537
      67   B 1999 CFESIS: x  0.083814116
      68   A 2000 CFESIS: x -0.009819537
      69   B 2000 CFESIS: x  0.083814116
      220  A 1951 CFESIS: x           NA
      221  B 1951 CFESIS: x           NA
      222  C 1951 CFESIS: x           NA
      223  A 1952 CFESIS: x           NA
      224  B 1952 CFESIS: x           NA
      225  C 1952 CFESIS: x           NA
      226  A 1953 CFESIS: x           NA
      227  B 1953 CFESIS: x           NA
      228  C 1953 CFESIS: x           NA
      229  A 1954 CFESIS: x           NA
      230  B 1954 CFESIS: x           NA
      231  C 1954 CFESIS: x           NA
      232  A 1955 CFESIS: x           NA
      233  B 1955 CFESIS: x           NA
      234  C 1955 CFESIS: x           NA
      235  A 1956 CFESIS: x           NA
      236  B 1956 CFESIS: x           NA
      237  C 1956 CFESIS: x           NA
      238  A 1957 CFESIS: x           NA
      239  B 1957 CFESIS: x           NA
      240  C 1957 CFESIS: x           NA
      241  A 1958 CFESIS: x           NA
      242  B 1958 CFESIS: x           NA
      243  C 1958 CFESIS: x           NA
      244  A 1959 CFESIS: x           NA
      245  B 1959 CFESIS: x           NA
      246  C 1959 CFESIS: x           NA
      247  C 1986 CFESIS: x           NA
      248  B 1960 CFESIS: x           NA
      249  C 1960 CFESIS: x           NA
      250  C 1987 CFESIS: x           NA
      251  B 1961 CFESIS: x           NA
      252  C 1961 CFESIS: x           NA
      253  C 1988 CFESIS: x           NA
      254  B 1962 CFESIS: x           NA
      255  C 1962 CFESIS: x           NA
      256  C 1989 CFESIS: x           NA
      257  B 1963 CFESIS: x           NA
      258  C 1963 CFESIS: x           NA
      259  C 1990 CFESIS: x           NA
      260  B 1964 CFESIS: x           NA
      261  C 1964 CFESIS: x           NA
      262  C 1991 CFESIS: x           NA
      263  B 1965 CFESIS: x           NA
      264  C 1965 CFESIS: x           NA
      265  C 1992 CFESIS: x           NA
      266  B 1966 CFESIS: x           NA
      267  C 1966 CFESIS: x           NA
      268  C 1993 CFESIS: x           NA
      269  B 1967 CFESIS: x           NA
      270  C 1967 CFESIS: x           NA
      271  C 1994 CFESIS: x           NA
      272  B 1968 CFESIS: x           NA
      273  C 1968 CFESIS: x           NA
      274  C 1995 CFESIS: x           NA
      275  B 1969 CFESIS: x           NA
      276  C 1969 CFESIS: x           NA
      277  C 1996 CFESIS: x           NA
      278  B 1970 CFESIS: x           NA
      279  C 1970 CFESIS: x           NA
      280  C 1997 CFESIS: x           NA
      281  B 1971 CFESIS: x           NA
      282  C 1971 CFESIS: x           NA
      283  C 1998 CFESIS: x           NA
      284  B 1972 CFESIS: x           NA
      285  C 1972 CFESIS: x           NA
      286  C 1999 CFESIS: x           NA
      287  C 1977 CFESIS: x           NA
      288  C 1973 CFESIS: x           NA
      289  C 2000 CFESIS: x           NA
      290  C 1978 CFESIS: x           NA
      291  C 1974 CFESIS: x           NA
      292  C 1983 CFESIS: x           NA
      293  C 1979 CFESIS: x           NA
      294  C 1975 CFESIS: x           NA
      295  C 1984 CFESIS: x           NA
      296  C 1980 CFESIS: x           NA
      297  C 1976 CFESIS: x           NA
      298  C 1985 CFESIS: x           NA
      299  C 1981 CFESIS: x           NA
      300  C 1982 CFESIS: x           NA
      
      $layers
      $layers$geom_tile
      geom_tile: na.rm = TRUE, lineend = butt, linejoin = mitre
      stat_identity: na.rm = TRUE
      position_identity 
      
      
      $scales
      <ggproto object: Class ScalesList, gg>
          add: function
          add_defaults: function
          add_missing: function
          backtransform_df: function
          clone: function
          find: function
          get_scales: function
          has_scale: function
          input: function
          map_df: function
          n: function
          non_position_scales: function
          scales: list
          set_palettes: function
          train_df: function
          transform_df: function
          super:  <ggproto object: Class ScalesList, gg>
      
      $guides
      <Guides[0] ggproto object>
      
      <empty>
      
      $mapping
      Aesthetic mapping: 
      * `x`    -> `.data$time`
      * `y`    -> `.data$id`
      * `fill` -> `.data$effect`
      
      $theme
      <theme> List of 144
       $ line                            : <ggplot2::element_line>
        ..@ colour       : chr "black"
        ..@ linewidth    : num 0.5
        ..@ linetype     : num 1
        ..@ lineend      : chr "butt"
        ..@ linejoin     : chr "round"
        ..@ arrow        : logi FALSE
        ..@ arrow.fill   : chr "black"
        ..@ inherit.blank: logi TRUE
       $ rect                            : <ggplot2::element_rect>
        ..@ fill         : chr "white"
        ..@ colour       : chr "black"
        ..@ linewidth    : num 0.5
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
        ..@ size         : num 11
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
        ..@ size         : num 1.5
        ..@ fill         : chr "white"
        ..@ stroke       : num 0.5
        ..@ inherit.blank: logi TRUE
       $ polygon                         : <ggplot2::element_polygon>
        ..@ fill         : chr "white"
        ..@ colour       : chr "black"
        ..@ linewidth    : num 0.5
        ..@ linetype     : num 1
        ..@ linejoin     : chr "round"
        ..@ inherit.blank: logi TRUE
       $ geom                            : <ggplot2::element_geom>
        ..@ ink        : chr "black"
        ..@ paper      : chr "white"
        ..@ accent     : chr "#3366FF"
        ..@ linewidth  : num 0.5
        ..@ borderwidth: num 0.5
        ..@ linetype   : int 1
        ..@ bordertype : int 1
        ..@ family     : chr ""
        ..@ fontsize   : num 3.87
        ..@ pointsize  : num 1.5
        ..@ pointshape : num 19
        ..@ colour     : NULL
        ..@ fill       : NULL
       $ spacing                         : 'simpleUnit' num 5.5points
        ..- attr(*, "unit")= int 8
       $ margins                         : <ggplot2::margin> num [1:4] 5.5 5.5 5.5 5.5
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
        ..@ margin       : <ggplot2::margin> num [1:4] 2.75 0 0 0
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
        ..@ margin       : <ggplot2::margin> num [1:4] 0 0 2.75 0
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
        ..@ margin       : <ggplot2::margin> num [1:4] 0 2.75 0 0
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
        ..@ margin       : <ggplot2::margin> num [1:4] 0 0 0 2.75
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
        ..@ colour       : NULL
        ..@ size         : NULL
        ..@ hjust        : NULL
        ..@ vjust        : num 1
        ..@ angle        : NULL
        ..@ lineheight   : NULL
        ..@ margin       : <ggplot2::margin> num [1:4] 2.2 0 0 0
        ..@ debug        : NULL
        ..@ inherit.blank: logi TRUE
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
        ..@ margin       : <ggplot2::margin> num [1:4] 0 0 2.2 0
        ..@ debug        : NULL
        ..@ inherit.blank: logi TRUE
       $ axis.text.x.bottom              : NULL
       $ axis.text.y                     : <ggplot2::element_text>
        ..@ family       : NULL
        ..@ face         : NULL
        ..@ italic       : chr NA
        ..@ fontweight   : num NA
        ..@ fontwidth    : num NA
        ..@ colour       : NULL
        ..@ size         : NULL
        ..@ hjust        : num 1
        ..@ vjust        : NULL
        ..@ angle        : NULL
        ..@ lineheight   : NULL
        ..@ margin       : <ggplot2::margin> num [1:4] 0 2.2 0 0
        ..@ debug        : NULL
        ..@ inherit.blank: logi TRUE
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
        ..@ margin       : <ggplot2::margin> num [1:4] 0 0 0 2.2
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
        ..@ margin       : <ggplot2::margin> num [1:4] 0 2.2 0 2.2
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
       $ axis.line                       : <ggplot2::element_blank>
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
      
      $coordinates
      <ggproto object: Class CoordCartesian, Coord, gg>
          aspect: function
          backtransform_range: function
          clip: on
          default: TRUE
          distance: function
          draw_panel: function
          expand: TRUE
          is_free: function
          is_linear: function
          labels: function
          limits: list
          modify_scales: function
          range: function
          ratio: NULL
          render_axis_h: function
          render_axis_v: function
          render_bg: function
          render_fg: function
          reverse: none
          setup_data: function
          setup_layout: function
          setup_panel_guides: function
          setup_panel_params: function
          setup_params: function
          train_panel_guides: function
          transform: function
          super:  <ggproto object: Class CoordCartesian, Coord, gg>
      
      $facet
      <ggproto object: Class FacetWrap, Facet, gg>
          attach_axes: function
          attach_strips: function
          compute_layout: function
          draw_back: function
          draw_front: function
          draw_labels: function
          draw_panel_content: function
          draw_panels: function
          finish_data: function
          format_strip_labels: function
          init_gtable: function
          init_scales: function
          map_data: function
          params: list
          set_panel_size: function
          setup_data: function
          setup_panel_params: function
          setup_params: function
          shrink: TRUE
          train_scales: function
          vars: function
          super:  <ggproto object: Class FacetWrap, Facet, gg>
      
      $layout
      <ggproto object: Class Layout, gg>
          coord: NULL
          coord_params: list
          facet: NULL
          facet_params: list
          finish_data: function
          get_scales: function
          layout: NULL
          map_position: function
          panel_params: NULL
          panel_scales_x: NULL
          panel_scales_y: NULL
          render: function
          render_labels: function
          reset_scales: function
          resolve_label: function
          setup: function
          setup_panel_guides: function
          setup_panel_params: function
          train_position: function
          super:  <ggproto object: Class Layout, gg>
      
      $labels
      <ggplot2::labels> List of 2
       $ x: NULL
       $ y: NULL
      
      $meta
      list()
      

---

    Code
      p_counter
    Output
      $data
          id time         y         x idB idC time1951 time1952 time1953 time1954
      1    A 1951 450.07731  99.43952   0   0        1        0        0        0
      2    A 1952 450.43872  99.76982   0   0        0        1        0        0
      3    A 1953 451.44579 101.55871   0   0        0        0        1        0
      4    A 1954 450.63358 100.07051   0   0        0        0        0        1
      5    A 1955 451.04075 100.12929   0   0        0        0        0        0
      6    A 1956 452.00145 101.71506   0   0        0        0        0        0
      7    A 1957 451.74306 100.46092   0   0        0        0        0        0
      8    A 1958 450.89298  98.73494   0   0        0        0        0        0
      9    A 1959 451.65197  99.31315   0   0        0        0        0        0
      10   A 1960 451.70225  99.55434   0   0        0        0        0        0
      11   A 1961 453.02258 101.22408   0   0        0        0        0        0
      12   A 1962 452.37007 100.35981   0   0        0        0        0        0
      13   A 1963 452.54835 100.40077   0   0        0        0        0        0
      14   A 1964 453.50355 100.11068   0   0        0        0        0        0
      15   A 1965 452.63871  99.44416   0   0        0        0        0        0
      16   A 1966 454.15310 101.78691   0   0        0        0        0        0
      17   A 1967 453.77624 100.49785   0   0        0        0        0        0
      18   A 1968 452.51994  98.03338   0   0        0        0        0        0
      19   A 1969 454.25405 100.70136   0   0        0        0        0        0
      20   A 1970 453.53740  99.52721   0   0        0        0        0        0
      21   A 1971 453.02301  98.93218   0   0        0        0        0        0
      22   A 1972 453.40407  99.78203   0   0        0        0        0        0
      23   A 1973 452.88018  98.97400   0   0        0        0        0        0
      24   A 1974 453.36124  99.27111   0   0        0        0        0        0
      25   A 1975 452.73921  99.37496   0   0        0        0        0        0
      26   A 1976 452.03745  98.31331   0   0        0        0        0        0
      27   A 1977 453.42645 100.83779   0   0        0        0        0        0
      28   A 1978 453.03878 100.15337   0   0        0        0        0        0
      29   A 1979 452.31824  98.86186   0   0        0        0        0        0
      30   A 1980 453.23523 101.25381   0   0        0        0        0        0
      31   A 1981 452.60057 100.42646   0   0        0        0        0        0
      32   A 1982 452.60510  99.70493   0   0        0        0        0        0
      33   A 1983 452.77763 100.89513   0   0        0        0        0        0
      34   A 1984 452.56596 100.87813   0   0        0        0        0        0
      35   A 1985 452.56353 100.82158   0   0        0        0        0        0
      36   A 1986 452.40488 100.68864   0   0        0        0        0        0
      37   A 1987 452.49894 100.55392   0   0        0        0        0        0
      38   A 1988 451.88599  99.93809   0   0        0        0        0        0
      39   A 1989 451.79783  99.69404   0   0        0        0        0        0
      40   A 1990 460.43810  99.61953   0   0        0        0        0        0
      41   A 1991 460.32145  99.30529   0   0        0        0        0        0
      42   A 1992 460.35773  99.79208   0   0        0        0        0        0
      43   A 1993 459.80194  98.73460   0   0        0        0        0        0
      44   A 1994 461.24951 102.16896   0   0        0        0        0        0
      45   A 1995 460.57266 101.20796   0   0        0        0        0        0
      46   A 1996 459.95665  98.87689   0   0        0        0        0        0
      47   A 1997 459.93707  99.59712   0   0        0        0        0        0
      48   A 1998 459.42475  99.53334   0   0        0        0        0        0
      49   A 1999 460.08910 100.77997   0   0        0        0        0        0
      50   A 2000 459.42964  99.91663   0   0        0        0        0        0
      51   B 1951 210.66642  30.25332   1   0        1        0        0        0
      52   B 1952 210.44821  29.97145   1   0        0        1        0        0
      53   B 1953 210.22554  29.95713   1   0        0        0        1        0
      54   B 1954 211.19294  31.36860   1   0        0        0        0        1
      55   B 1955 210.30425  29.77423   1   0        0        0        0        0
      56   B 1956 211.26299  31.51647   1   0        0        0        0        0
      57   B 1957 209.76790  28.45125   1   0        0        0        0        0
      58   B 1958 210.97338  30.58461   1   0        0        0        0        0
      59   B 1959 211.29211  30.12385   1   0        0        0        0        0
      60   B 1960 211.09717  30.21594   1   0        0        0        0        0
      61   B 1961 211.31367  30.37964   1   0        0        0        0        0
      62   B 1962 210.99758  29.49768   1   0        0        0        0        0
      63   B 1963 211.37989  29.66679   1   0        0        0        0        0
      64   B 1964 210.78750  28.98142   1   0        0        0        0        0
      65   B 1965 210.76560  28.92821   1   0        0        0        0        0
      66   B 1966 212.08690  30.30353   1   0        0        0        0        0
      67   B 1967 211.83587  30.44821   1   0        0        0        0        0
      68   B 1968 211.68189  30.05300   1   0        0        0        0        0
      69   B 1969 212.11388  30.92227   1   0        0        0        0        0
      70   B 1970 212.76810  32.05008   1   0        0        0        0        0
      71   B 1971 211.73969  29.50897   1   0        0        0        0        0
      72   B 1972 211.16901  27.69083   1   0        0        0        0        0
      73   B 1973 213.02484  31.00574   1   0        0        0        0        0
      74   B 1974 212.18692  29.29080   1   0        0        0        0        0
      75   B 1975 212.08326  29.31199   1   0        0        0        0        0
      76   B 1976 213.12474  31.02557   1   0        0        0        0        0
      77   B 1977 212.41669  29.71523   1   0        0        0        0        0
      78   B 1978 212.04620  28.77928   1   0        0        0        0        0
      79   B 1979 213.16758  30.18130   1   0        0        0        0        0
      80   B 1980 212.72744  29.86111   1   0        0        0        0        0
      81   B 1981 213.49394  30.00576   1   0        0        0        0        0
      82   B 1982 213.37458  30.38528   1   0        0        0        0        0
      83   B 1983 213.15758  29.62934   1   0        0        0        0        0
      84   B 1984 213.57448  30.64438   1   0        0        0        0        0
      85   B 1985 213.27488  29.77951   1   0        0        0        0        0
      86   B 1986 213.50249  30.33178   1   0        0        0        0        0
      87   B 1987 214.21183  31.09684   1   0        0        0        0        0
      88   B 1988 214.10139  30.43518   1   0        0        0        0        0
      89   B 1989 213.80190  29.67407   1   0        0        0        0        0
      90   B 1990 214.41810  31.14881   1   0        0        0        0        0
      91   B 1991 214.43903  30.99350   1   0        0        0        0        0
      92   B 1992 214.37376  30.54840   1   0        0        0        0        0
      93   B 1993 214.71858  30.23873   1   0        0        0        0        0
      94   B 1994 213.85859  29.37209   1   0        0        0        0        0
      95   B 1995 215.14452  31.36065   1   0        0        0        0        0
      96   B 1996 214.68034  29.39974   1   0        0        0        0        0
      97   B 1997 215.77347  32.18733   1   0        0        0        0        0
      98   B 1998 215.29434  31.53261   1   0        0        0        0        0
      99   B 1999 214.64920  29.76430   1   0        0        0        0        0
      100  B 2000 214.58388  28.97358   1   0        0        0        0        0
      101  C 1951  34.66968  69.28959   0   1        1        0        0        0
      102  C 1952  35.11607  70.25688   0   1        0        1        0        0
      103  C 1953  34.90787  69.75331   0   1        0        0        1        0
      104  C 1954  34.94433  69.65246   0   1        0        0        0        1
      105  C 1955  34.94389  69.04838   0   1        0        0        0        0
      106  C 1956  35.05977  69.95497   0   1        0        0        0        0
      107  C 1957  34.92371  69.21510   0   1        0        0        0        0
      108  C 1958  34.39218  68.33206   0   1        0        0        0        0
      109  C 1959  34.88716  69.61977   0   1        0        0        0        0
      110  C 1960  35.25292  70.91900   0   1        0        0        0        0
      111  C 1961  34.70810  69.42465   0   1        0        0        0        0
      112  C 1962  35.30601  70.60796   0   1        0        0        0        0
      113  C 1963  34.30049  68.38212   0   1        0        0        0        0
      114  C 1964  35.33226  69.94444   0   1        0        0        0        0
      115  C 1965  35.81832  70.51941   0   1        0        0        0        0
      116  C 1966  35.56009  70.30115   0   1        0        0        0        0
      117  C 1967  35.12621  70.10568   0   1        0        0        0        0
      118  C 1968  34.42834  69.35929   0   1        0        0        0        0
      119  C 1969  34.59739  69.15030   0   1        0        0        0        0
      120  C 1970  34.60578  68.97587   0   1        0        0        0        0
      121  C 1971  35.32783  70.11765   0   1        0        0        0        0
      122  C 1972  34.81877  69.05253   0   1        0        0        0        0
      123  C 1973  34.99158  69.50944   0   1        0        0        0        0
      124  C 1974  34.69290  69.74391   0   1        0        0        0        0
      125  C 1975  36.19186  71.84386   0   1        0        0        0        0
      126  C 1976  34.68471  69.34805   0   1        0        0        0        0
      127  C 1977  35.25265  70.23539   0   1        0        0        0        0
      128  C 1978  35.15389  70.07796   0   1        0        0        0        0
      129  C 1979  34.70471  69.03814   0   1        0        0        0        0
      130  C 1980  35.06928  69.92869   0   1        0        0        0        0
      131  C 1981  35.48878  71.44455   0   1        0        0        0        0
      132  C 1982  35.47305  70.45150   0   1        0        0        0        0
      133  C 1983  35.19782  70.04123   0   1        0        0        0        0
      134  C 1984  34.83562  69.57750   0   1        0        0        0        0
      135  C 1985  34.09701  67.94675   0   1        0        0        0        0
      136  C 1986  35.69248  71.13134   0   1        0        0        0        0
      137  C 1987  34.41388  68.53936   0   1        0        0        0        0
      138  C 1988  35.79814  70.73995   0   1        0        0        0        0
      139  C 1989  36.01074  71.90910   0   1        0        0        0        0
      140  C 1990  34.41167  68.55611   0   1        0        0        0        0
      141  C 1991  35.68457  70.70178   0   1        0        0        0        0
      142  C 1992  35.17974  69.73780   0   1        0        0        0        0
      143  C 1993  34.54298  68.42786   0   1        0        0        0        0
      144  C 1994  34.22717  68.48533   0   1        0        0        0        0
      145  C 1995  34.69973  68.39846   0   1        0        0        0        0
      146  C 1996  34.84789  69.46909   0   1        0        0        0        0
      147  C 1997  34.74249  68.53824   0   1        0        0        0        0
      148  C 1998  35.17378  70.68792   0   1        0        0        0        0
      149  C 1999  36.15425  72.10011   0   1        0        0        0        0
      150  C 2000  34.70647  68.71297   0   1        0        0        0        0
          time1955 time1956 time1957 time1958 time1959 time1960 time1961 time1962
      1          0        0        0        0        0        0        0        0
      2          0        0        0        0        0        0        0        0
      3          0        0        0        0        0        0        0        0
      4          0        0        0        0        0        0        0        0
      5          1        0        0        0        0        0        0        0
      6          0        1        0        0        0        0        0        0
      7          0        0        1        0        0        0        0        0
      8          0        0        0        1        0        0        0        0
      9          0        0        0        0        1        0        0        0
      10         0        0        0        0        0        1        0        0
      11         0        0        0        0        0        0        1        0
      12         0        0        0        0        0        0        0        1
      13         0        0        0        0        0        0        0        0
      14         0        0        0        0        0        0        0        0
      15         0        0        0        0        0        0        0        0
      16         0        0        0        0        0        0        0        0
      17         0        0        0        0        0        0        0        0
      18         0        0        0        0        0        0        0        0
      19         0        0        0        0        0        0        0        0
      20         0        0        0        0        0        0        0        0
      21         0        0        0        0        0        0        0        0
      22         0        0        0        0        0        0        0        0
      23         0        0        0        0        0        0        0        0
      24         0        0        0        0        0        0        0        0
      25         0        0        0        0        0        0        0        0
      26         0        0        0        0        0        0        0        0
      27         0        0        0        0        0        0        0        0
      28         0        0        0        0        0        0        0        0
      29         0        0        0        0        0        0        0        0
      30         0        0        0        0        0        0        0        0
      31         0        0        0        0        0        0        0        0
      32         0        0        0        0        0        0        0        0
      33         0        0        0        0        0        0        0        0
      34         0        0        0        0        0        0        0        0
      35         0        0        0        0        0        0        0        0
      36         0        0        0        0        0        0        0        0
      37         0        0        0        0        0        0        0        0
      38         0        0        0        0        0        0        0        0
      39         0        0        0        0        0        0        0        0
      40         0        0        0        0        0        0        0        0
      41         0        0        0        0        0        0        0        0
      42         0        0        0        0        0        0        0        0
      43         0        0        0        0        0        0        0        0
      44         0        0        0        0        0        0        0        0
      45         0        0        0        0        0        0        0        0
      46         0        0        0        0        0        0        0        0
      47         0        0        0        0        0        0        0        0
      48         0        0        0        0        0        0        0        0
      49         0        0        0        0        0        0        0        0
      50         0        0        0        0        0        0        0        0
      51         0        0        0        0        0        0        0        0
      52         0        0        0        0        0        0        0        0
      53         0        0        0        0        0        0        0        0
      54         0        0        0        0        0        0        0        0
      55         1        0        0        0        0        0        0        0
      56         0        1        0        0        0        0        0        0
      57         0        0        1        0        0        0        0        0
      58         0        0        0        1        0        0        0        0
      59         0        0        0        0        1        0        0        0
      60         0        0        0        0        0        1        0        0
      61         0        0        0        0        0        0        1        0
      62         0        0        0        0        0        0        0        1
      63         0        0        0        0        0        0        0        0
      64         0        0        0        0        0        0        0        0
      65         0        0        0        0        0        0        0        0
      66         0        0        0        0        0        0        0        0
      67         0        0        0        0        0        0        0        0
      68         0        0        0        0        0        0        0        0
      69         0        0        0        0        0        0        0        0
      70         0        0        0        0        0        0        0        0
      71         0        0        0        0        0        0        0        0
      72         0        0        0        0        0        0        0        0
      73         0        0        0        0        0        0        0        0
      74         0        0        0        0        0        0        0        0
      75         0        0        0        0        0        0        0        0
      76         0        0        0        0        0        0        0        0
      77         0        0        0        0        0        0        0        0
      78         0        0        0        0        0        0        0        0
      79         0        0        0        0        0        0        0        0
      80         0        0        0        0        0        0        0        0
      81         0        0        0        0        0        0        0        0
      82         0        0        0        0        0        0        0        0
      83         0        0        0        0        0        0        0        0
      84         0        0        0        0        0        0        0        0
      85         0        0        0        0        0        0        0        0
      86         0        0        0        0        0        0        0        0
      87         0        0        0        0        0        0        0        0
      88         0        0        0        0        0        0        0        0
      89         0        0        0        0        0        0        0        0
      90         0        0        0        0        0        0        0        0
      91         0        0        0        0        0        0        0        0
      92         0        0        0        0        0        0        0        0
      93         0        0        0        0        0        0        0        0
      94         0        0        0        0        0        0        0        0
      95         0        0        0        0        0        0        0        0
      96         0        0        0        0        0        0        0        0
      97         0        0        0        0        0        0        0        0
      98         0        0        0        0        0        0        0        0
      99         0        0        0        0        0        0        0        0
      100        0        0        0        0        0        0        0        0
      101        0        0        0        0        0        0        0        0
      102        0        0        0        0        0        0        0        0
      103        0        0        0        0        0        0        0        0
      104        0        0        0        0        0        0        0        0
      105        1        0        0        0        0        0        0        0
      106        0        1        0        0        0        0        0        0
      107        0        0        1        0        0        0        0        0
      108        0        0        0        1        0        0        0        0
      109        0        0        0        0        1        0        0        0
      110        0        0        0        0        0        1        0        0
      111        0        0        0        0        0        0        1        0
      112        0        0        0        0        0        0        0        1
      113        0        0        0        0        0        0        0        0
      114        0        0        0        0        0        0        0        0
      115        0        0        0        0        0        0        0        0
      116        0        0        0        0        0        0        0        0
      117        0        0        0        0        0        0        0        0
      118        0        0        0        0        0        0        0        0
      119        0        0        0        0        0        0        0        0
      120        0        0        0        0        0        0        0        0
      121        0        0        0        0        0        0        0        0
      122        0        0        0        0        0        0        0        0
      123        0        0        0        0        0        0        0        0
      124        0        0        0        0        0        0        0        0
      125        0        0        0        0        0        0        0        0
      126        0        0        0        0        0        0        0        0
      127        0        0        0        0        0        0        0        0
      128        0        0        0        0        0        0        0        0
      129        0        0        0        0        0        0        0        0
      130        0        0        0        0        0        0        0        0
      131        0        0        0        0        0        0        0        0
      132        0        0        0        0        0        0        0        0
      133        0        0        0        0        0        0        0        0
      134        0        0        0        0        0        0        0        0
      135        0        0        0        0        0        0        0        0
      136        0        0        0        0        0        0        0        0
      137        0        0        0        0        0        0        0        0
      138        0        0        0        0        0        0        0        0
      139        0        0        0        0        0        0        0        0
      140        0        0        0        0        0        0        0        0
      141        0        0        0        0        0        0        0        0
      142        0        0        0        0        0        0        0        0
      143        0        0        0        0        0        0        0        0
      144        0        0        0        0        0        0        0        0
      145        0        0        0        0        0        0        0        0
      146        0        0        0        0        0        0        0        0
      147        0        0        0        0        0        0        0        0
      148        0        0        0        0        0        0        0        0
      149        0        0        0        0        0        0        0        0
      150        0        0        0        0        0        0        0        0
          time1963 time1964 time1965 time1966 time1967 time1968 time1969 time1970
      1          0        0        0        0        0        0        0        0
      2          0        0        0        0        0        0        0        0
      3          0        0        0        0        0        0        0        0
      4          0        0        0        0        0        0        0        0
      5          0        0        0        0        0        0        0        0
      6          0        0        0        0        0        0        0        0
      7          0        0        0        0        0        0        0        0
      8          0        0        0        0        0        0        0        0
      9          0        0        0        0        0        0        0        0
      10         0        0        0        0        0        0        0        0
      11         0        0        0        0        0        0        0        0
      12         0        0        0        0        0        0        0        0
      13         1        0        0        0        0        0        0        0
      14         0        1        0        0        0        0        0        0
      15         0        0        1        0        0        0        0        0
      16         0        0        0        1        0        0        0        0
      17         0        0        0        0        1        0        0        0
      18         0        0        0        0        0        1        0        0
      19         0        0        0        0        0        0        1        0
      20         0        0        0        0        0        0        0        1
      21         0        0        0        0        0        0        0        0
      22         0        0        0        0        0        0        0        0
      23         0        0        0        0        0        0        0        0
      24         0        0        0        0        0        0        0        0
      25         0        0        0        0        0        0        0        0
      26         0        0        0        0        0        0        0        0
      27         0        0        0        0        0        0        0        0
      28         0        0        0        0        0        0        0        0
      29         0        0        0        0        0        0        0        0
      30         0        0        0        0        0        0        0        0
      31         0        0        0        0        0        0        0        0
      32         0        0        0        0        0        0        0        0
      33         0        0        0        0        0        0        0        0
      34         0        0        0        0        0        0        0        0
      35         0        0        0        0        0        0        0        0
      36         0        0        0        0        0        0        0        0
      37         0        0        0        0        0        0        0        0
      38         0        0        0        0        0        0        0        0
      39         0        0        0        0        0        0        0        0
      40         0        0        0        0        0        0        0        0
      41         0        0        0        0        0        0        0        0
      42         0        0        0        0        0        0        0        0
      43         0        0        0        0        0        0        0        0
      44         0        0        0        0        0        0        0        0
      45         0        0        0        0        0        0        0        0
      46         0        0        0        0        0        0        0        0
      47         0        0        0        0        0        0        0        0
      48         0        0        0        0        0        0        0        0
      49         0        0        0        0        0        0        0        0
      50         0        0        0        0        0        0        0        0
      51         0        0        0        0        0        0        0        0
      52         0        0        0        0        0        0        0        0
      53         0        0        0        0        0        0        0        0
      54         0        0        0        0        0        0        0        0
      55         0        0        0        0        0        0        0        0
      56         0        0        0        0        0        0        0        0
      57         0        0        0        0        0        0        0        0
      58         0        0        0        0        0        0        0        0
      59         0        0        0        0        0        0        0        0
      60         0        0        0        0        0        0        0        0
      61         0        0        0        0        0        0        0        0
      62         0        0        0        0        0        0        0        0
      63         1        0        0        0        0        0        0        0
      64         0        1        0        0        0        0        0        0
      65         0        0        1        0        0        0        0        0
      66         0        0        0        1        0        0        0        0
      67         0        0        0        0        1        0        0        0
      68         0        0        0        0        0        1        0        0
      69         0        0        0        0        0        0        1        0
      70         0        0        0        0        0        0        0        1
      71         0        0        0        0        0        0        0        0
      72         0        0        0        0        0        0        0        0
      73         0        0        0        0        0        0        0        0
      74         0        0        0        0        0        0        0        0
      75         0        0        0        0        0        0        0        0
      76         0        0        0        0        0        0        0        0
      77         0        0        0        0        0        0        0        0
      78         0        0        0        0        0        0        0        0
      79         0        0        0        0        0        0        0        0
      80         0        0        0        0        0        0        0        0
      81         0        0        0        0        0        0        0        0
      82         0        0        0        0        0        0        0        0
      83         0        0        0        0        0        0        0        0
      84         0        0        0        0        0        0        0        0
      85         0        0        0        0        0        0        0        0
      86         0        0        0        0        0        0        0        0
      87         0        0        0        0        0        0        0        0
      88         0        0        0        0        0        0        0        0
      89         0        0        0        0        0        0        0        0
      90         0        0        0        0        0        0        0        0
      91         0        0        0        0        0        0        0        0
      92         0        0        0        0        0        0        0        0
      93         0        0        0        0        0        0        0        0
      94         0        0        0        0        0        0        0        0
      95         0        0        0        0        0        0        0        0
      96         0        0        0        0        0        0        0        0
      97         0        0        0        0        0        0        0        0
      98         0        0        0        0        0        0        0        0
      99         0        0        0        0        0        0        0        0
      100        0        0        0        0        0        0        0        0
      101        0        0        0        0        0        0        0        0
      102        0        0        0        0        0        0        0        0
      103        0        0        0        0        0        0        0        0
      104        0        0        0        0        0        0        0        0
      105        0        0        0        0        0        0        0        0
      106        0        0        0        0        0        0        0        0
      107        0        0        0        0        0        0        0        0
      108        0        0        0        0        0        0        0        0
      109        0        0        0        0        0        0        0        0
      110        0        0        0        0        0        0        0        0
      111        0        0        0        0        0        0        0        0
      112        0        0        0        0        0        0        0        0
      113        1        0        0        0        0        0        0        0
      114        0        1        0        0        0        0        0        0
      115        0        0        1        0        0        0        0        0
      116        0        0        0        1        0        0        0        0
      117        0        0        0        0        1        0        0        0
      118        0        0        0        0        0        1        0        0
      119        0        0        0        0        0        0        1        0
      120        0        0        0        0        0        0        0        1
      121        0        0        0        0        0        0        0        0
      122        0        0        0        0        0        0        0        0
      123        0        0        0        0        0        0        0        0
      124        0        0        0        0        0        0        0        0
      125        0        0        0        0        0        0        0        0
      126        0        0        0        0        0        0        0        0
      127        0        0        0        0        0        0        0        0
      128        0        0        0        0        0        0        0        0
      129        0        0        0        0        0        0        0        0
      130        0        0        0        0        0        0        0        0
      131        0        0        0        0        0        0        0        0
      132        0        0        0        0        0        0        0        0
      133        0        0        0        0        0        0        0        0
      134        0        0        0        0        0        0        0        0
      135        0        0        0        0        0        0        0        0
      136        0        0        0        0        0        0        0        0
      137        0        0        0        0        0        0        0        0
      138        0        0        0        0        0        0        0        0
      139        0        0        0        0        0        0        0        0
      140        0        0        0        0        0        0        0        0
      141        0        0        0        0        0        0        0        0
      142        0        0        0        0        0        0        0        0
      143        0        0        0        0        0        0        0        0
      144        0        0        0        0        0        0        0        0
      145        0        0        0        0        0        0        0        0
      146        0        0        0        0        0        0        0        0
      147        0        0        0        0        0        0        0        0
      148        0        0        0        0        0        0        0        0
      149        0        0        0        0        0        0        0        0
      150        0        0        0        0        0        0        0        0
          time1971 time1972 time1973 time1974 time1975 time1976 time1977 time1978
      1          0        0        0        0        0        0        0        0
      2          0        0        0        0        0        0        0        0
      3          0        0        0        0        0        0        0        0
      4          0        0        0        0        0        0        0        0
      5          0        0        0        0        0        0        0        0
      6          0        0        0        0        0        0        0        0
      7          0        0        0        0        0        0        0        0
      8          0        0        0        0        0        0        0        0
      9          0        0        0        0        0        0        0        0
      10         0        0        0        0        0        0        0        0
      11         0        0        0        0        0        0        0        0
      12         0        0        0        0        0        0        0        0
      13         0        0        0        0        0        0        0        0
      14         0        0        0        0        0        0        0        0
      15         0        0        0        0        0        0        0        0
      16         0        0        0        0        0        0        0        0
      17         0        0        0        0        0        0        0        0
      18         0        0        0        0        0        0        0        0
      19         0        0        0        0        0        0        0        0
      20         0        0        0        0        0        0        0        0
      21         1        0        0        0        0        0        0        0
      22         0        1        0        0        0        0        0        0
      23         0        0        1        0        0        0        0        0
      24         0        0        0        1        0        0        0        0
      25         0        0        0        0        1        0        0        0
      26         0        0        0        0        0        1        0        0
      27         0        0        0        0        0        0        1        0
      28         0        0        0        0        0        0        0        1
      29         0        0        0        0        0        0        0        0
      30         0        0        0        0        0        0        0        0
      31         0        0        0        0        0        0        0        0
      32         0        0        0        0        0        0        0        0
      33         0        0        0        0        0        0        0        0
      34         0        0        0        0        0        0        0        0
      35         0        0        0        0        0        0        0        0
      36         0        0        0        0        0        0        0        0
      37         0        0        0        0        0        0        0        0
      38         0        0        0        0        0        0        0        0
      39         0        0        0        0        0        0        0        0
      40         0        0        0        0        0        0        0        0
      41         0        0        0        0        0        0        0        0
      42         0        0        0        0        0        0        0        0
      43         0        0        0        0        0        0        0        0
      44         0        0        0        0        0        0        0        0
      45         0        0        0        0        0        0        0        0
      46         0        0        0        0        0        0        0        0
      47         0        0        0        0        0        0        0        0
      48         0        0        0        0        0        0        0        0
      49         0        0        0        0        0        0        0        0
      50         0        0        0        0        0        0        0        0
      51         0        0        0        0        0        0        0        0
      52         0        0        0        0        0        0        0        0
      53         0        0        0        0        0        0        0        0
      54         0        0        0        0        0        0        0        0
      55         0        0        0        0        0        0        0        0
      56         0        0        0        0        0        0        0        0
      57         0        0        0        0        0        0        0        0
      58         0        0        0        0        0        0        0        0
      59         0        0        0        0        0        0        0        0
      60         0        0        0        0        0        0        0        0
      61         0        0        0        0        0        0        0        0
      62         0        0        0        0        0        0        0        0
      63         0        0        0        0        0        0        0        0
      64         0        0        0        0        0        0        0        0
      65         0        0        0        0        0        0        0        0
      66         0        0        0        0        0        0        0        0
      67         0        0        0        0        0        0        0        0
      68         0        0        0        0        0        0        0        0
      69         0        0        0        0        0        0        0        0
      70         0        0        0        0        0        0        0        0
      71         1        0        0        0        0        0        0        0
      72         0        1        0        0        0        0        0        0
      73         0        0        1        0        0        0        0        0
      74         0        0        0        1        0        0        0        0
      75         0        0        0        0        1        0        0        0
      76         0        0        0        0        0        1        0        0
      77         0        0        0        0        0        0        1        0
      78         0        0        0        0        0        0        0        1
      79         0        0        0        0        0        0        0        0
      80         0        0        0        0        0        0        0        0
      81         0        0        0        0        0        0        0        0
      82         0        0        0        0        0        0        0        0
      83         0        0        0        0        0        0        0        0
      84         0        0        0        0        0        0        0        0
      85         0        0        0        0        0        0        0        0
      86         0        0        0        0        0        0        0        0
      87         0        0        0        0        0        0        0        0
      88         0        0        0        0        0        0        0        0
      89         0        0        0        0        0        0        0        0
      90         0        0        0        0        0        0        0        0
      91         0        0        0        0        0        0        0        0
      92         0        0        0        0        0        0        0        0
      93         0        0        0        0        0        0        0        0
      94         0        0        0        0        0        0        0        0
      95         0        0        0        0        0        0        0        0
      96         0        0        0        0        0        0        0        0
      97         0        0        0        0        0        0        0        0
      98         0        0        0        0        0        0        0        0
      99         0        0        0        0        0        0        0        0
      100        0        0        0        0        0        0        0        0
      101        0        0        0        0        0        0        0        0
      102        0        0        0        0        0        0        0        0
      103        0        0        0        0        0        0        0        0
      104        0        0        0        0        0        0        0        0
      105        0        0        0        0        0        0        0        0
      106        0        0        0        0        0        0        0        0
      107        0        0        0        0        0        0        0        0
      108        0        0        0        0        0        0        0        0
      109        0        0        0        0        0        0        0        0
      110        0        0        0        0        0        0        0        0
      111        0        0        0        0        0        0        0        0
      112        0        0        0        0        0        0        0        0
      113        0        0        0        0        0        0        0        0
      114        0        0        0        0        0        0        0        0
      115        0        0        0        0        0        0        0        0
      116        0        0        0        0        0        0        0        0
      117        0        0        0        0        0        0        0        0
      118        0        0        0        0        0        0        0        0
      119        0        0        0        0        0        0        0        0
      120        0        0        0        0        0        0        0        0
      121        1        0        0        0        0        0        0        0
      122        0        1        0        0        0        0        0        0
      123        0        0        1        0        0        0        0        0
      124        0        0        0        1        0        0        0        0
      125        0        0        0        0        1        0        0        0
      126        0        0        0        0        0        1        0        0
      127        0        0        0        0        0        0        1        0
      128        0        0        0        0        0        0        0        1
      129        0        0        0        0        0        0        0        0
      130        0        0        0        0        0        0        0        0
      131        0        0        0        0        0        0        0        0
      132        0        0        0        0        0        0        0        0
      133        0        0        0        0        0        0        0        0
      134        0        0        0        0        0        0        0        0
      135        0        0        0        0        0        0        0        0
      136        0        0        0        0        0        0        0        0
      137        0        0        0        0        0        0        0        0
      138        0        0        0        0        0        0        0        0
      139        0        0        0        0        0        0        0        0
      140        0        0        0        0        0        0        0        0
      141        0        0        0        0        0        0        0        0
      142        0        0        0        0        0        0        0        0
      143        0        0        0        0        0        0        0        0
      144        0        0        0        0        0        0        0        0
      145        0        0        0        0        0        0        0        0
      146        0        0        0        0        0        0        0        0
      147        0        0        0        0        0        0        0        0
      148        0        0        0        0        0        0        0        0
      149        0        0        0        0        0        0        0        0
      150        0        0        0        0        0        0        0        0
          time1979 time1980 time1981 time1982 time1983 time1984 time1985 time1986
      1          0        0        0        0        0        0        0        0
      2          0        0        0        0        0        0        0        0
      3          0        0        0        0        0        0        0        0
      4          0        0        0        0        0        0        0        0
      5          0        0        0        0        0        0        0        0
      6          0        0        0        0        0        0        0        0
      7          0        0        0        0        0        0        0        0
      8          0        0        0        0        0        0        0        0
      9          0        0        0        0        0        0        0        0
      10         0        0        0        0        0        0        0        0
      11         0        0        0        0        0        0        0        0
      12         0        0        0        0        0        0        0        0
      13         0        0        0        0        0        0        0        0
      14         0        0        0        0        0        0        0        0
      15         0        0        0        0        0        0        0        0
      16         0        0        0        0        0        0        0        0
      17         0        0        0        0        0        0        0        0
      18         0        0        0        0        0        0        0        0
      19         0        0        0        0        0        0        0        0
      20         0        0        0        0        0        0        0        0
      21         0        0        0        0        0        0        0        0
      22         0        0        0        0        0        0        0        0
      23         0        0        0        0        0        0        0        0
      24         0        0        0        0        0        0        0        0
      25         0        0        0        0        0        0        0        0
      26         0        0        0        0        0        0        0        0
      27         0        0        0        0        0        0        0        0
      28         0        0        0        0        0        0        0        0
      29         1        0        0        0        0        0        0        0
      30         0        1        0        0        0        0        0        0
      31         0        0        1        0        0        0        0        0
      32         0        0        0        1        0        0        0        0
      33         0        0        0        0        1        0        0        0
      34         0        0        0        0        0        1        0        0
      35         0        0        0        0        0        0        1        0
      36         0        0        0        0        0        0        0        1
      37         0        0        0        0        0        0        0        0
      38         0        0        0        0        0        0        0        0
      39         0        0        0        0        0        0        0        0
      40         0        0        0        0        0        0        0        0
      41         0        0        0        0        0        0        0        0
      42         0        0        0        0        0        0        0        0
      43         0        0        0        0        0        0        0        0
      44         0        0        0        0        0        0        0        0
      45         0        0        0        0        0        0        0        0
      46         0        0        0        0        0        0        0        0
      47         0        0        0        0        0        0        0        0
      48         0        0        0        0        0        0        0        0
      49         0        0        0        0        0        0        0        0
      50         0        0        0        0        0        0        0        0
      51         0        0        0        0        0        0        0        0
      52         0        0        0        0        0        0        0        0
      53         0        0        0        0        0        0        0        0
      54         0        0        0        0        0        0        0        0
      55         0        0        0        0        0        0        0        0
      56         0        0        0        0        0        0        0        0
      57         0        0        0        0        0        0        0        0
      58         0        0        0        0        0        0        0        0
      59         0        0        0        0        0        0        0        0
      60         0        0        0        0        0        0        0        0
      61         0        0        0        0        0        0        0        0
      62         0        0        0        0        0        0        0        0
      63         0        0        0        0        0        0        0        0
      64         0        0        0        0        0        0        0        0
      65         0        0        0        0        0        0        0        0
      66         0        0        0        0        0        0        0        0
      67         0        0        0        0        0        0        0        0
      68         0        0        0        0        0        0        0        0
      69         0        0        0        0        0        0        0        0
      70         0        0        0        0        0        0        0        0
      71         0        0        0        0        0        0        0        0
      72         0        0        0        0        0        0        0        0
      73         0        0        0        0        0        0        0        0
      74         0        0        0        0        0        0        0        0
      75         0        0        0        0        0        0        0        0
      76         0        0        0        0        0        0        0        0
      77         0        0        0        0        0        0        0        0
      78         0        0        0        0        0        0        0        0
      79         1        0        0        0        0        0        0        0
      80         0        1        0        0        0        0        0        0
      81         0        0        1        0        0        0        0        0
      82         0        0        0        1        0        0        0        0
      83         0        0        0        0        1        0        0        0
      84         0        0        0        0        0        1        0        0
      85         0        0        0        0        0        0        1        0
      86         0        0        0        0        0        0        0        1
      87         0        0        0        0        0        0        0        0
      88         0        0        0        0        0        0        0        0
      89         0        0        0        0        0        0        0        0
      90         0        0        0        0        0        0        0        0
      91         0        0        0        0        0        0        0        0
      92         0        0        0        0        0        0        0        0
      93         0        0        0        0        0        0        0        0
      94         0        0        0        0        0        0        0        0
      95         0        0        0        0        0        0        0        0
      96         0        0        0        0        0        0        0        0
      97         0        0        0        0        0        0        0        0
      98         0        0        0        0        0        0        0        0
      99         0        0        0        0        0        0        0        0
      100        0        0        0        0        0        0        0        0
      101        0        0        0        0        0        0        0        0
      102        0        0        0        0        0        0        0        0
      103        0        0        0        0        0        0        0        0
      104        0        0        0        0        0        0        0        0
      105        0        0        0        0        0        0        0        0
      106        0        0        0        0        0        0        0        0
      107        0        0        0        0        0        0        0        0
      108        0        0        0        0        0        0        0        0
      109        0        0        0        0        0        0        0        0
      110        0        0        0        0        0        0        0        0
      111        0        0        0        0        0        0        0        0
      112        0        0        0        0        0        0        0        0
      113        0        0        0        0        0        0        0        0
      114        0        0        0        0        0        0        0        0
      115        0        0        0        0        0        0        0        0
      116        0        0        0        0        0        0        0        0
      117        0        0        0        0        0        0        0        0
      118        0        0        0        0        0        0        0        0
      119        0        0        0        0        0        0        0        0
      120        0        0        0        0        0        0        0        0
      121        0        0        0        0        0        0        0        0
      122        0        0        0        0        0        0        0        0
      123        0        0        0        0        0        0        0        0
      124        0        0        0        0        0        0        0        0
      125        0        0        0        0        0        0        0        0
      126        0        0        0        0        0        0        0        0
      127        0        0        0        0        0        0        0        0
      128        0        0        0        0        0        0        0        0
      129        1        0        0        0        0        0        0        0
      130        0        1        0        0        0        0        0        0
      131        0        0        1        0        0        0        0        0
      132        0        0        0        1        0        0        0        0
      133        0        0        0        0        1        0        0        0
      134        0        0        0        0        0        1        0        0
      135        0        0        0        0        0        0        1        0
      136        0        0        0        0        0        0        0        1
      137        0        0        0        0        0        0        0        0
      138        0        0        0        0        0        0        0        0
      139        0        0        0        0        0        0        0        0
      140        0        0        0        0        0        0        0        0
      141        0        0        0        0        0        0        0        0
      142        0        0        0        0        0        0        0        0
      143        0        0        0        0        0        0        0        0
      144        0        0        0        0        0        0        0        0
      145        0        0        0        0        0        0        0        0
      146        0        0        0        0        0        0        0        0
      147        0        0        0        0        0        0        0        0
      148        0        0        0        0        0        0        0        0
      149        0        0        0        0        0        0        0        0
      150        0        0        0        0        0        0        0        0
          time1987 time1988 time1989 time1990 time1991 time1992 time1993 time1994
      1          0        0        0        0        0        0        0        0
      2          0        0        0        0        0        0        0        0
      3          0        0        0        0        0        0        0        0
      4          0        0        0        0        0        0        0        0
      5          0        0        0        0        0        0        0        0
      6          0        0        0        0        0        0        0        0
      7          0        0        0        0        0        0        0        0
      8          0        0        0        0        0        0        0        0
      9          0        0        0        0        0        0        0        0
      10         0        0        0        0        0        0        0        0
      11         0        0        0        0        0        0        0        0
      12         0        0        0        0        0        0        0        0
      13         0        0        0        0        0        0        0        0
      14         0        0        0        0        0        0        0        0
      15         0        0        0        0        0        0        0        0
      16         0        0        0        0        0        0        0        0
      17         0        0        0        0        0        0        0        0
      18         0        0        0        0        0        0        0        0
      19         0        0        0        0        0        0        0        0
      20         0        0        0        0        0        0        0        0
      21         0        0        0        0        0        0        0        0
      22         0        0        0        0        0        0        0        0
      23         0        0        0        0        0        0        0        0
      24         0        0        0        0        0        0        0        0
      25         0        0        0        0        0        0        0        0
      26         0        0        0        0        0        0        0        0
      27         0        0        0        0        0        0        0        0
      28         0        0        0        0        0        0        0        0
      29         0        0        0        0        0        0        0        0
      30         0        0        0        0        0        0        0        0
      31         0        0        0        0        0        0        0        0
      32         0        0        0        0        0        0        0        0
      33         0        0        0        0        0        0        0        0
      34         0        0        0        0        0        0        0        0
      35         0        0        0        0        0        0        0        0
      36         0        0        0        0        0        0        0        0
      37         1        0        0        0        0        0        0        0
      38         0        1        0        0        0        0        0        0
      39         0        0        1        0        0        0        0        0
      40         0        0        0        1        0        0        0        0
      41         0        0        0        0        1        0        0        0
      42         0        0        0        0        0        1        0        0
      43         0        0        0        0        0        0        1        0
      44         0        0        0        0        0        0        0        1
      45         0        0        0        0        0        0        0        0
      46         0        0        0        0        0        0        0        0
      47         0        0        0        0        0        0        0        0
      48         0        0        0        0        0        0        0        0
      49         0        0        0        0        0        0        0        0
      50         0        0        0        0        0        0        0        0
      51         0        0        0        0        0        0        0        0
      52         0        0        0        0        0        0        0        0
      53         0        0        0        0        0        0        0        0
      54         0        0        0        0        0        0        0        0
      55         0        0        0        0        0        0        0        0
      56         0        0        0        0        0        0        0        0
      57         0        0        0        0        0        0        0        0
      58         0        0        0        0        0        0        0        0
      59         0        0        0        0        0        0        0        0
      60         0        0        0        0        0        0        0        0
      61         0        0        0        0        0        0        0        0
      62         0        0        0        0        0        0        0        0
      63         0        0        0        0        0        0        0        0
      64         0        0        0        0        0        0        0        0
      65         0        0        0        0        0        0        0        0
      66         0        0        0        0        0        0        0        0
      67         0        0        0        0        0        0        0        0
      68         0        0        0        0        0        0        0        0
      69         0        0        0        0        0        0        0        0
      70         0        0        0        0        0        0        0        0
      71         0        0        0        0        0        0        0        0
      72         0        0        0        0        0        0        0        0
      73         0        0        0        0        0        0        0        0
      74         0        0        0        0        0        0        0        0
      75         0        0        0        0        0        0        0        0
      76         0        0        0        0        0        0        0        0
      77         0        0        0        0        0        0        0        0
      78         0        0        0        0        0        0        0        0
      79         0        0        0        0        0        0        0        0
      80         0        0        0        0        0        0        0        0
      81         0        0        0        0        0        0        0        0
      82         0        0        0        0        0        0        0        0
      83         0        0        0        0        0        0        0        0
      84         0        0        0        0        0        0        0        0
      85         0        0        0        0        0        0        0        0
      86         0        0        0        0        0        0        0        0
      87         1        0        0        0        0        0        0        0
      88         0        1        0        0        0        0        0        0
      89         0        0        1        0        0        0        0        0
      90         0        0        0        1        0        0        0        0
      91         0        0        0        0        1        0        0        0
      92         0        0        0        0        0        1        0        0
      93         0        0        0        0        0        0        1        0
      94         0        0        0        0        0        0        0        1
      95         0        0        0        0        0        0        0        0
      96         0        0        0        0        0        0        0        0
      97         0        0        0        0        0        0        0        0
      98         0        0        0        0        0        0        0        0
      99         0        0        0        0        0        0        0        0
      100        0        0        0        0        0        0        0        0
      101        0        0        0        0        0        0        0        0
      102        0        0        0        0        0        0        0        0
      103        0        0        0        0        0        0        0        0
      104        0        0        0        0        0        0        0        0
      105        0        0        0        0        0        0        0        0
      106        0        0        0        0        0        0        0        0
      107        0        0        0        0        0        0        0        0
      108        0        0        0        0        0        0        0        0
      109        0        0        0        0        0        0        0        0
      110        0        0        0        0        0        0        0        0
      111        0        0        0        0        0        0        0        0
      112        0        0        0        0        0        0        0        0
      113        0        0        0        0        0        0        0        0
      114        0        0        0        0        0        0        0        0
      115        0        0        0        0        0        0        0        0
      116        0        0        0        0        0        0        0        0
      117        0        0        0        0        0        0        0        0
      118        0        0        0        0        0        0        0        0
      119        0        0        0        0        0        0        0        0
      120        0        0        0        0        0        0        0        0
      121        0        0        0        0        0        0        0        0
      122        0        0        0        0        0        0        0        0
      123        0        0        0        0        0        0        0        0
      124        0        0        0        0        0        0        0        0
      125        0        0        0        0        0        0        0        0
      126        0        0        0        0        0        0        0        0
      127        0        0        0        0        0        0        0        0
      128        0        0        0        0        0        0        0        0
      129        0        0        0        0        0        0        0        0
      130        0        0        0        0        0        0        0        0
      131        0        0        0        0        0        0        0        0
      132        0        0        0        0        0        0        0        0
      133        0        0        0        0        0        0        0        0
      134        0        0        0        0        0        0        0        0
      135        0        0        0        0        0        0        0        0
      136        0        0        0        0        0        0        0        0
      137        1        0        0        0        0        0        0        0
      138        0        1        0        0        0        0        0        0
      139        0        0        1        0        0        0        0        0
      140        0        0        0        1        0        0        0        0
      141        0        0        0        0        1        0        0        0
      142        0        0        0        0        0        1        0        0
      143        0        0        0        0        0        0        1        0
      144        0        0        0        0        0        0        0        1
      145        0        0        0        0        0        0        0        0
      146        0        0        0        0        0        0        0        0
      147        0        0        0        0        0        0        0        0
      148        0        0        0        0        0        0        0        0
      149        0        0        0        0        0        0        0        0
      150        0        0        0        0        0        0        0        0
          time1995 time1996 time1997 time1998 time1999 time2000 fesisA.1981
      1          0        0        0        0        0        0           0
      2          0        0        0        0        0        0           0
      3          0        0        0        0        0        0           0
      4          0        0        0        0        0        0           0
      5          0        0        0        0        0        0           0
      6          0        0        0        0        0        0           0
      7          0        0        0        0        0        0           0
      8          0        0        0        0        0        0           0
      9          0        0        0        0        0        0           0
      10         0        0        0        0        0        0           0
      11         0        0        0        0        0        0           0
      12         0        0        0        0        0        0           0
      13         0        0        0        0        0        0           0
      14         0        0        0        0        0        0           0
      15         0        0        0        0        0        0           0
      16         0        0        0        0        0        0           0
      17         0        0        0        0        0        0           0
      18         0        0        0        0        0        0           0
      19         0        0        0        0        0        0           0
      20         0        0        0        0        0        0           0
      21         0        0        0        0        0        0           0
      22         0        0        0        0        0        0           0
      23         0        0        0        0        0        0           0
      24         0        0        0        0        0        0           0
      25         0        0        0        0        0        0           0
      26         0        0        0        0        0        0           0
      27         0        0        0        0        0        0           0
      28         0        0        0        0        0        0           0
      29         0        0        0        0        0        0           0
      30         0        0        0        0        0        0           0
      31         0        0        0        0        0        0           1
      32         0        0        0        0        0        0           1
      33         0        0        0        0        0        0           1
      34         0        0        0        0        0        0           1
      35         0        0        0        0        0        0           1
      36         0        0        0        0        0        0           1
      37         0        0        0        0        0        0           1
      38         0        0        0        0        0        0           1
      39         0        0        0        0        0        0           1
      40         0        0        0        0        0        0           1
      41         0        0        0        0        0        0           1
      42         0        0        0        0        0        0           1
      43         0        0        0        0        0        0           1
      44         0        0        0        0        0        0           1
      45         1        0        0        0        0        0           1
      46         0        1        0        0        0        0           1
      47         0        0        1        0        0        0           1
      48         0        0        0        1        0        0           1
      49         0        0        0        0        1        0           1
      50         0        0        0        0        0        1           1
      51         0        0        0        0        0        0           0
      52         0        0        0        0        0        0           0
      53         0        0        0        0        0        0           0
      54         0        0        0        0        0        0           0
      55         0        0        0        0        0        0           0
      56         0        0        0        0        0        0           0
      57         0        0        0        0        0        0           0
      58         0        0        0        0        0        0           0
      59         0        0        0        0        0        0           0
      60         0        0        0        0        0        0           0
      61         0        0        0        0        0        0           0
      62         0        0        0        0        0        0           0
      63         0        0        0        0        0        0           0
      64         0        0        0        0        0        0           0
      65         0        0        0        0        0        0           0
      66         0        0        0        0        0        0           0
      67         0        0        0        0        0        0           0
      68         0        0        0        0        0        0           0
      69         0        0        0        0        0        0           0
      70         0        0        0        0        0        0           0
      71         0        0        0        0        0        0           0
      72         0        0        0        0        0        0           0
      73         0        0        0        0        0        0           0
      74         0        0        0        0        0        0           0
      75         0        0        0        0        0        0           0
      76         0        0        0        0        0        0           0
      77         0        0        0        0        0        0           0
      78         0        0        0        0        0        0           0
      79         0        0        0        0        0        0           0
      80         0        0        0        0        0        0           0
      81         0        0        0        0        0        0           0
      82         0        0        0        0        0        0           0
      83         0        0        0        0        0        0           0
      84         0        0        0        0        0        0           0
      85         0        0        0        0        0        0           0
      86         0        0        0        0        0        0           0
      87         0        0        0        0        0        0           0
      88         0        0        0        0        0        0           0
      89         0        0        0        0        0        0           0
      90         0        0        0        0        0        0           0
      91         0        0        0        0        0        0           0
      92         0        0        0        0        0        0           0
      93         0        0        0        0        0        0           0
      94         0        0        0        0        0        0           0
      95         1        0        0        0        0        0           0
      96         0        1        0        0        0        0           0
      97         0        0        1        0        0        0           0
      98         0        0        0        1        0        0           0
      99         0        0        0        0        1        0           0
      100        0        0        0        0        0        1           0
      101        0        0        0        0        0        0           0
      102        0        0        0        0        0        0           0
      103        0        0        0        0        0        0           0
      104        0        0        0        0        0        0           0
      105        0        0        0        0        0        0           0
      106        0        0        0        0        0        0           0
      107        0        0        0        0        0        0           0
      108        0        0        0        0        0        0           0
      109        0        0        0        0        0        0           0
      110        0        0        0        0        0        0           0
      111        0        0        0        0        0        0           0
      112        0        0        0        0        0        0           0
      113        0        0        0        0        0        0           0
      114        0        0        0        0        0        0           0
      115        0        0        0        0        0        0           0
      116        0        0        0        0        0        0           0
      117        0        0        0        0        0        0           0
      118        0        0        0        0        0        0           0
      119        0        0        0        0        0        0           0
      120        0        0        0        0        0        0           0
      121        0        0        0        0        0        0           0
      122        0        0        0        0        0        0           0
      123        0        0        0        0        0        0           0
      124        0        0        0        0        0        0           0
      125        0        0        0        0        0        0           0
      126        0        0        0        0        0        0           0
      127        0        0        0        0        0        0           0
      128        0        0        0        0        0        0           0
      129        0        0        0        0        0        0           0
      130        0        0        0        0        0        0           0
      131        0        0        0        0        0        0           0
      132        0        0        0        0        0        0           0
      133        0        0        0        0        0        0           0
      134        0        0        0        0        0        0           0
      135        0        0        0        0        0        0           0
      136        0        0        0        0        0        0           0
      137        0        0        0        0        0        0           0
      138        0        0        0        0        0        0           0
      139        0        0        0        0        0        0           0
      140        0        0        0        0        0        0           0
      141        0        0        0        0        0        0           0
      142        0        0        0        0        0        0           0
      143        0        0        0        0        0        0           0
      144        0        0        0        0        0        0           0
      145        1        0        0        0        0        0           0
      146        0        1        0        0        0        0           0
      147        0        0        1        0        0        0           0
      148        0        0        0        1        0        0           0
      149        0        0        0        0        1        0           0
      150        0        0        0        0        0        1           0
          fesisA.1990 fesisC.1973 fesisC.1990
      1             0           0           0
      2             0           0           0
      3             0           0           0
      4             0           0           0
      5             0           0           0
      6             0           0           0
      7             0           0           0
      8             0           0           0
      9             0           0           0
      10            0           0           0
      11            0           0           0
      12            0           0           0
      13            0           0           0
      14            0           0           0
      15            0           0           0
      16            0           0           0
      17            0           0           0
      18            0           0           0
      19            0           0           0
      20            0           0           0
      21            0           0           0
      22            0           0           0
      23            0           0           0
      24            0           0           0
      25            0           0           0
      26            0           0           0
      27            0           0           0
      28            0           0           0
      29            0           0           0
      30            0           0           0
      31            0           0           0
      32            0           0           0
      33            0           0           0
      34            0           0           0
      35            0           0           0
      36            0           0           0
      37            0           0           0
      38            0           0           0
      39            0           0           0
      40            1           0           0
      41            1           0           0
      42            1           0           0
      43            1           0           0
      44            1           0           0
      45            1           0           0
      46            1           0           0
      47            1           0           0
      48            1           0           0
      49            1           0           0
      50            1           0           0
      51            0           0           0
      52            0           0           0
      53            0           0           0
      54            0           0           0
      55            0           0           0
      56            0           0           0
      57            0           0           0
      58            0           0           0
      59            0           0           0
      60            0           0           0
      61            0           0           0
      62            0           0           0
      63            0           0           0
      64            0           0           0
      65            0           0           0
      66            0           0           0
      67            0           0           0
      68            0           0           0
      69            0           0           0
      70            0           0           0
      71            0           0           0
      72            0           0           0
      73            0           0           0
      74            0           0           0
      75            0           0           0
      76            0           0           0
      77            0           0           0
      78            0           0           0
      79            0           0           0
      80            0           0           0
      81            0           0           0
      82            0           0           0
      83            0           0           0
      84            0           0           0
      85            0           0           0
      86            0           0           0
      87            0           0           0
      88            0           0           0
      89            0           0           0
      90            0           0           0
      91            0           0           0
      92            0           0           0
      93            0           0           0
      94            0           0           0
      95            0           0           0
      96            0           0           0
      97            0           0           0
      98            0           0           0
      99            0           0           0
      100           0           0           0
      101           0           0           0
      102           0           0           0
      103           0           0           0
      104           0           0           0
      105           0           0           0
      106           0           0           0
      107           0           0           0
      108           0           0           0
      109           0           0           0
      110           0           0           0
      111           0           0           0
      112           0           0           0
      113           0           0           0
      114           0           0           0
      115           0           0           0
      116           0           0           0
      117           0           0           0
      118           0           0           0
      119           0           0           0
      120           0           0           0
      121           0           0           0
      122           0           0           0
      123           0           1           0
      124           0           1           0
      125           0           1           0
      126           0           1           0
      127           0           1           0
      128           0           1           0
      129           0           1           0
      130           0           1           0
      131           0           1           0
      132           0           1           0
      133           0           1           0
      134           0           1           0
      135           0           1           0
      136           0           1           0
      137           0           1           0
      138           0           1           0
      139           0           1           0
      140           0           1           1
      141           0           1           1
      142           0           1           1
      143           0           1           1
      144           0           1           1
      145           0           1           1
      146           0           1           1
      147           0           1           1
      148           0           1           1
      149           0           1           1
      150           0           1           1
      
      $layers
      $layers$geom_line
      mapping: y = ~.data$y, colour = black 
      geom_line: na.rm = FALSE, orientation = NA, arrow = NULL, arrow.fill = NULL, lineend = butt, linejoin = round, linemitre = 10
      stat_identity: na.rm = FALSE
      position_identity 
      
      $layers$geom_rect
      mapping: xmin = ~.data$start_rect, xmax = ~.data$end_rect, ymin = ~-Inf, ymax = Inf, group = ~.data$name 
      geom_rect: na.rm = TRUE, lineend = butt, linejoin = mitre
      stat_identity: na.rm = TRUE
      position_identity 
      
      $layers$geom_line...3
      mapping: colour = blue 
      geom_line: na.rm = FALSE, orientation = NA, arrow = NULL, arrow.fill = NULL, lineend = butt, linejoin = round, linemitre = 10
      stat_identity: na.rm = FALSE
      position_identity 
      
      $layers$geom_vline
      mapping: xintercept = ~.data$time, colour = red 
      geom_vline: na.rm = FALSE
      stat_identity: na.rm = FALSE
      position_identity 
      
      $layers$geom_ribbon
      mapping: ymin = ~.data$cf_lwr, ymax = ~.data$cf_upr, fill = red, group = ~.data$name 
      geom_ribbon: na.rm = FALSE, orientation = NA, lineend = butt, linejoin = round, linemitre = 10, outline.type = both
      stat_identity: na.rm = FALSE
      position_identity 
      
      $layers$geom_line...6
      mapping: y = ~.data$cf, colour = red, group = ~.data$name 
      geom_line: na.rm = TRUE, orientation = NA, arrow = NULL, arrow.fill = NULL, lineend = butt, linejoin = round, linemitre = 10
      stat_identity: na.rm = TRUE
      position_identity 
      
      
      $scales
      <ggproto object: Class ScalesList, gg>
          add: function
          add_defaults: function
          add_missing: function
          backtransform_df: function
          clone: function
          find: function
          get_scales: function
          has_scale: function
          input: function
          map_df: function
          n: function
          non_position_scales: function
          scales: list
          set_palettes: function
          train_df: function
          transform_df: function
          super:  <ggproto object: Class ScalesList, gg>
      
      $guides
      <Guides[1] ggproto object>
      
      fill : "none"
      
      $mapping
      Aesthetic mapping: 
      * `x`     -> `.data$time`
      * `y`     -> `fitted`
      * `group` -> `.data$id`
      
      $theme
      <theme> List of 4
       $ legend.key      : <ggplot2::element_rect>
        ..@ fill         : logi NA
        ..@ colour       : NULL
        ..@ linewidth    : NULL
        ..@ linetype     : NULL
        ..@ linejoin     : NULL
        ..@ inherit.blank: logi FALSE
       $ panel.background: <ggplot2::element_blank>
       $ panel.border    : <ggplot2::element_rect>
        ..@ fill         : logi NA
        ..@ colour       : chr "grey"
        ..@ linewidth    : NULL
        ..@ linetype     : NULL
        ..@ linejoin     : NULL
        ..@ inherit.blank: logi FALSE
       $ strip.background: <ggplot2::element_blank>
       @ complete: logi FALSE
       @ validate: logi TRUE
      
      $coordinates
      <ggproto object: Class CoordCartesian, Coord, gg>
          aspect: function
          backtransform_range: function
          clip: on
          default: TRUE
          distance: function
          draw_panel: function
          expand: TRUE
          is_free: function
          is_linear: function
          labels: function
          limits: list
          modify_scales: function
          range: function
          ratio: NULL
          render_axis_h: function
          render_axis_v: function
          render_bg: function
          render_fg: function
          reverse: none
          setup_data: function
          setup_layout: function
          setup_panel_guides: function
          setup_panel_params: function
          setup_params: function
          train_panel_guides: function
          transform: function
          super:  <ggproto object: Class CoordCartesian, Coord, gg>
      
      $facet
      <ggproto object: Class FacetWrap, Facet, gg>
          attach_axes: function
          attach_strips: function
          compute_layout: function
          draw_back: function
          draw_front: function
          draw_labels: function
          draw_panel_content: function
          draw_panels: function
          finish_data: function
          format_strip_labels: function
          init_gtable: function
          init_scales: function
          map_data: function
          params: list
          set_panel_size: function
          setup_data: function
          setup_panel_params: function
          setup_params: function
          shrink: TRUE
          train_scales: function
          vars: function
          super:  <ggproto object: Class FacetWrap, Facet, gg>
      
      $layout
      <ggproto object: Class Layout, gg>
          coord: NULL
          coord_params: list
          facet: NULL
          facet_params: list
          finish_data: function
          get_scales: function
          layout: NULL
          map_position: function
          panel_params: NULL
          panel_scales_x: NULL
          panel_scales_y: NULL
          render: function
          render_labels: function
          reset_scales: function
          resolve_label: function
          setup: function
          setup_panel_guides: function
          setup_panel_params: function
          train_position: function
          super:  <ggproto object: Class Layout, gg>
      
      $labels
      <ggplot2::labels> List of 4
       $ y       : NULL
       $ x       : NULL
       $ title   : NULL
       $ subtitle: NULL
      
      $meta
      list()
      

