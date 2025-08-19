# Test that regex_exclude_indicators works

    Code
      p
    Output
      $data
                      id time coef       effect      model
      1          Austria 1995   NA           NA  0.05; CO2
      2          Austria 1996   NA           NA  0.05; CO2
      3          Austria 1997   NA           NA  0.05; CO2
      4          Austria 1998   NA           NA  0.05; CO2
      5          Austria 1999   NA           NA  0.05; CO2
      6          Austria 2000   NA           NA  0.05; CO2
      7          Austria 2001   NA           NA  0.05; CO2
      8          Austria 2002   NA  0.128055298  0.05; CO2
      9          Austria 2003   NA  0.128055298  0.05; CO2
      10         Austria 2004   NA  0.128055298  0.05; CO2
      11         Austria 2005   NA  0.128055298  0.05; CO2
      12         Austria 2006   NA  0.128055298  0.05; CO2
      13         Austria 2007   NA  0.128055298  0.05; CO2
      14         Austria 2008   NA  0.128055298  0.05; CO2
      15         Austria 2009   NA  0.128055298  0.05; CO2
      16         Austria 2010   NA  0.128055298  0.05; CO2
      17         Austria 2011   NA  0.128055298  0.05; CO2
      18         Austria 2012   NA  0.128055298  0.05; CO2
      19         Austria 2013   NA  0.217469398  0.05; CO2
      20         Austria 2014   NA  0.217469398  0.05; CO2
      21         Austria 2015   NA  0.217469398  0.05; CO2
      22         Austria 2016   NA  0.217469398  0.05; CO2
      23         Austria 2017   NA  0.217469398  0.05; CO2
      24         Austria 2018   NA  0.217469398  0.05; CO2
      25         Belgium 1995   NA           NA  0.05; CO2
      26         Belgium 1996   NA           NA  0.05; CO2
      27         Belgium 1997   NA           NA  0.05; CO2
      28         Belgium 1998   NA           NA  0.05; CO2
      29         Belgium 1999   NA           NA  0.05; CO2
      30         Belgium 2000   NA           NA  0.05; CO2
      31         Belgium 2001   NA           NA  0.05; CO2
      32         Belgium 2002   NA           NA  0.05; CO2
      33         Belgium 2003   NA           NA  0.05; CO2
      34         Belgium 2004   NA           NA  0.05; CO2
      35         Belgium 2005   NA           NA  0.05; CO2
      36         Belgium 2006   NA           NA  0.05; CO2
      37         Belgium 2007   NA           NA  0.05; CO2
      38         Belgium 2008   NA           NA  0.05; CO2
      39         Belgium 2009   NA           NA  0.05; CO2
      40         Belgium 2010   NA           NA  0.05; CO2
      41         Belgium 2011   NA           NA  0.05; CO2
      42         Belgium 2012   NA           NA  0.05; CO2
      43         Belgium 2013   NA           NA  0.05; CO2
      44         Belgium 2014   NA           NA  0.05; CO2
      45         Belgium 2015   NA           NA  0.05; CO2
      46         Belgium 2016   NA           NA  0.05; CO2
      47         Belgium 2017   NA           NA  0.05; CO2
      48         Belgium 2018   NA           NA  0.05; CO2
      49         Denmark 1995   NA           NA  0.05; CO2
      50         Denmark 1996   NA           NA  0.05; CO2
      51         Denmark 1997   NA           NA  0.05; CO2
      52         Denmark 1998   NA           NA  0.05; CO2
      53         Denmark 1999   NA           NA  0.05; CO2
      54         Denmark 2000   NA           NA  0.05; CO2
      55         Denmark 2001   NA           NA  0.05; CO2
      56         Denmark 2002   NA           NA  0.05; CO2
      57         Denmark 2003   NA           NA  0.05; CO2
      58         Denmark 2004   NA           NA  0.05; CO2
      59         Denmark 2005   NA           NA  0.05; CO2
      60         Denmark 2006   NA           NA  0.05; CO2
      61         Denmark 2007   NA           NA  0.05; CO2
      62         Denmark 2008   NA           NA  0.05; CO2
      63         Denmark 2009   NA           NA  0.05; CO2
      64         Denmark 2010   NA           NA  0.05; CO2
      65         Denmark 2011   NA           NA  0.05; CO2
      66         Denmark 2012   NA           NA  0.05; CO2
      67         Denmark 2013   NA           NA  0.05; CO2
      68         Denmark 2014   NA           NA  0.05; CO2
      69         Denmark 2015   NA           NA  0.05; CO2
      70         Denmark 2016   NA           NA  0.05; CO2
      71         Denmark 2017   NA           NA  0.05; CO2
      72         Denmark 2018   NA           NA  0.05; CO2
      73         Finland 1995   NA           NA  0.05; CO2
      74         Finland 1996   NA           NA  0.05; CO2
      75         Finland 1997   NA           NA  0.05; CO2
      76         Finland 1998   NA           NA  0.05; CO2
      77         Finland 1999   NA           NA  0.05; CO2
      78         Finland 2000   NA -0.103350059  0.05; CO2
      79         Finland 2001   NA -0.103350059  0.05; CO2
      80         Finland 2002   NA -0.103350059  0.05; CO2
      81         Finland 2003   NA -0.103350059  0.05; CO2
      82         Finland 2004   NA -0.103350059  0.05; CO2
      83         Finland 2005   NA -0.103350059  0.05; CO2
      84         Finland 2006   NA -0.103350059  0.05; CO2
      85         Finland 2007   NA -0.103350059  0.05; CO2
      86         Finland 2008   NA -0.103350059  0.05; CO2
      87         Finland 2009   NA -0.103350059  0.05; CO2
      88         Finland 2010   NA -0.103350059  0.05; CO2
      89         Finland 2011   NA -0.103350059  0.05; CO2
      90         Finland 2012   NA -0.103350059  0.05; CO2
      91         Finland 2013   NA -0.103350059  0.05; CO2
      92         Finland 2014   NA -0.103350059  0.05; CO2
      93         Finland 2015   NA -0.103350059  0.05; CO2
      94         Finland 2016   NA -0.103350059  0.05; CO2
      95         Finland 2017   NA -0.103350059  0.05; CO2
      96         Finland 2018   NA -0.103350059  0.05; CO2
      97          France 1995   NA           NA  0.05; CO2
      98          France 1996   NA           NA  0.05; CO2
      99          France 1997   NA           NA  0.05; CO2
      100         France 1998   NA           NA  0.05; CO2
      101         France 1999   NA           NA  0.05; CO2
      102         France 2000   NA           NA  0.05; CO2
      103         France 2001   NA           NA  0.05; CO2
      104         France 2002   NA           NA  0.05; CO2
      105         France 2003   NA           NA  0.05; CO2
      106         France 2004   NA           NA  0.05; CO2
      107         France 2005   NA           NA  0.05; CO2
      108         France 2006   NA           NA  0.05; CO2
      109         France 2007   NA           NA  0.05; CO2
      110         France 2008   NA           NA  0.05; CO2
      111         France 2009   NA           NA  0.05; CO2
      112         France 2010   NA           NA  0.05; CO2
      113         France 2011   NA           NA  0.05; CO2
      114         France 2012   NA  0.074583924  0.05; CO2
      115         France 2013   NA  0.074583924  0.05; CO2
      116         France 2014   NA  0.074583924  0.05; CO2
      117         France 2015   NA  0.074583924  0.05; CO2
      118         France 2016   NA  0.074583924  0.05; CO2
      119         France 2017   NA  0.074583924  0.05; CO2
      120         France 2018   NA  0.074583924  0.05; CO2
      121        Germany 1995   NA           NA  0.05; CO2
      122        Germany 1996   NA           NA  0.05; CO2
      123        Germany 1997   NA           NA  0.05; CO2
      124        Germany 1998   NA           NA  0.05; CO2
      125        Germany 1999   NA           NA  0.05; CO2
      126        Germany 2000   NA           NA  0.05; CO2
      127        Germany 2001   NA           NA  0.05; CO2
      128        Germany 2002   NA -0.104903533  0.05; CO2
      129        Germany 2003   NA -0.104903533  0.05; CO2
      130        Germany 2004   NA -0.104903533  0.05; CO2
      131        Germany 2005   NA -0.104903533  0.05; CO2
      132        Germany 2006   NA -0.104903533  0.05; CO2
      133        Germany 2007   NA -0.104903533  0.05; CO2
      134        Germany 2008   NA -0.104903533  0.05; CO2
      135        Germany 2009   NA -0.104903533  0.05; CO2
      136        Germany 2010   NA -0.104903533  0.05; CO2
      137        Germany 2011   NA -0.104903533  0.05; CO2
      138        Germany 2012   NA  0.058055588  0.05; CO2
      139        Germany 2013   NA  0.058055588  0.05; CO2
      140        Germany 2014   NA  0.058055588  0.05; CO2
      141        Germany 2015   NA  0.058055588  0.05; CO2
      142        Germany 2016   NA  0.058055588  0.05; CO2
      143        Germany 2017   NA  0.058055588  0.05; CO2
      144        Germany 2018   NA  0.058055588  0.05; CO2
      145         Greece 1995   NA           NA  0.05; CO2
      146         Greece 1996   NA           NA  0.05; CO2
      147         Greece 1997   NA           NA  0.05; CO2
      148         Greece 1998   NA           NA  0.05; CO2
      149         Greece 1999   NA           NA  0.05; CO2
      150         Greece 2000   NA           NA  0.05; CO2
      151         Greece 2001   NA           NA  0.05; CO2
      152         Greece 2002   NA           NA  0.05; CO2
      153         Greece 2003   NA           NA  0.05; CO2
      154         Greece 2004   NA           NA  0.05; CO2
      155         Greece 2005   NA           NA  0.05; CO2
      156         Greece 2006   NA           NA  0.05; CO2
      157         Greece 2007   NA           NA  0.05; CO2
      158         Greece 2008   NA           NA  0.05; CO2
      159         Greece 2009   NA  0.093095910  0.05; CO2
      160         Greece 2010   NA  0.093095910  0.05; CO2
      161         Greece 2011   NA  0.093095910  0.05; CO2
      162         Greece 2012   NA  0.093095910  0.05; CO2
      163         Greece 2013   NA  0.093095910  0.05; CO2
      164         Greece 2014   NA  0.093095910  0.05; CO2
      165         Greece 2015   NA  0.093095910  0.05; CO2
      166         Greece 2016   NA  0.093095910  0.05; CO2
      167         Greece 2017   NA  0.093095910  0.05; CO2
      168         Greece 2018   NA  0.093095910  0.05; CO2
      169        Ireland 1995   NA           NA  0.05; CO2
      170        Ireland 1996   NA           NA  0.05; CO2
      171        Ireland 1997   NA           NA  0.05; CO2
      172        Ireland 1998   NA  0.099335110  0.05; CO2
      173        Ireland 1999   NA  0.099335110  0.05; CO2
      174        Ireland 2000   NA  0.099335110  0.05; CO2
      175        Ireland 2001   NA  0.099335110  0.05; CO2
      176        Ireland 2002   NA  0.099335110  0.05; CO2
      177        Ireland 2003   NA  0.099335110  0.05; CO2
      178        Ireland 2004   NA  0.099335110  0.05; CO2
      179        Ireland 2005   NA  0.099335110  0.05; CO2
      180        Ireland 2006   NA  0.099335110  0.05; CO2
      181        Ireland 2007   NA  0.099335110  0.05; CO2
      182        Ireland 2008   NA  0.099335110  0.05; CO2
      183        Ireland 2009   NA  0.099335110  0.05; CO2
      184        Ireland 2010   NA  0.099335110  0.05; CO2
      185        Ireland 2011   NA  0.012691226  0.05; CO2
      186        Ireland 2012   NA  0.012691226  0.05; CO2
      187        Ireland 2013   NA  0.012691226  0.05; CO2
      188        Ireland 2014   NA  0.012691226  0.05; CO2
      189        Ireland 2015   NA -0.134900995  0.05; CO2
      190        Ireland 2016   NA -0.134900995  0.05; CO2
      191        Ireland 2017   NA -0.134900995  0.05; CO2
      192        Ireland 2018   NA -0.134900995  0.05; CO2
      193          Italy 1995   NA           NA  0.05; CO2
      194          Italy 1996   NA           NA  0.05; CO2
      195          Italy 1997   NA           NA  0.05; CO2
      196          Italy 1998   NA           NA  0.05; CO2
      197          Italy 1999   NA           NA  0.05; CO2
      198          Italy 2000   NA           NA  0.05; CO2
      199          Italy 2001   NA           NA  0.05; CO2
      200          Italy 2002   NA           NA  0.05; CO2
      201          Italy 2003   NA           NA  0.05; CO2
      202          Italy 2004   NA           NA  0.05; CO2
      203          Italy 2005   NA           NA  0.05; CO2
      204          Italy 2006   NA           NA  0.05; CO2
      205          Italy 2007   NA           NA  0.05; CO2
      206          Italy 2008   NA           NA  0.05; CO2
      207          Italy 2009   NA           NA  0.05; CO2
      208          Italy 2010   NA           NA  0.05; CO2
      209          Italy 2011   NA           NA  0.05; CO2
      210          Italy 2012   NA           NA  0.05; CO2
      211          Italy 2013   NA           NA  0.05; CO2
      212          Italy 2014   NA           NA  0.05; CO2
      213          Italy 2015   NA           NA  0.05; CO2
      214          Italy 2016   NA           NA  0.05; CO2
      215          Italy 2017   NA           NA  0.05; CO2
      216          Italy 2018   NA           NA  0.05; CO2
      217     Luxembourg 1995   NA           NA  0.05; CO2
      218     Luxembourg 1996   NA           NA  0.05; CO2
      219     Luxembourg 1997   NA           NA  0.05; CO2
      220     Luxembourg 1998   NA           NA  0.05; CO2
      221     Luxembourg 1999   NA           NA  0.05; CO2
      222     Luxembourg 2000   NA           NA  0.05; CO2
      223     Luxembourg 2001   NA           NA  0.05; CO2
      224     Luxembourg 2002   NA           NA  0.05; CO2
      225     Luxembourg 2003   NA           NA  0.05; CO2
      226     Luxembourg 2004   NA  0.149018761  0.05; CO2
      227     Luxembourg 2005   NA  0.149018761  0.05; CO2
      228     Luxembourg 2006   NA  0.149018761  0.05; CO2
      229     Luxembourg 2007   NA  0.012749728  0.05; CO2
      230     Luxembourg 2008   NA  0.012749728  0.05; CO2
      231     Luxembourg 2009   NA  0.012749728  0.05; CO2
      232     Luxembourg 2010   NA  0.012749728  0.05; CO2
      233     Luxembourg 2011   NA  0.012749728  0.05; CO2
      234     Luxembourg 2012   NA  0.012749728  0.05; CO2
      235     Luxembourg 2013   NA  0.012749728  0.05; CO2
      236     Luxembourg 2014   NA  0.012749728  0.05; CO2
      237     Luxembourg 2015   NA  0.012749728  0.05; CO2
      238     Luxembourg 2016   NA  0.012749728  0.05; CO2
      239     Luxembourg 2017   NA  0.012749728  0.05; CO2
      240     Luxembourg 2018   NA  0.012749728  0.05; CO2
      241    Netherlands 1995   NA           NA  0.05; CO2
      242    Netherlands 1996   NA           NA  0.05; CO2
      243    Netherlands 1997   NA           NA  0.05; CO2
      244    Netherlands 1998   NA           NA  0.05; CO2
      245    Netherlands 1999   NA           NA  0.05; CO2
      246    Netherlands 2000   NA           NA  0.05; CO2
      247    Netherlands 2001   NA           NA  0.05; CO2
      248    Netherlands 2002   NA           NA  0.05; CO2
      249    Netherlands 2003   NA           NA  0.05; CO2
      250    Netherlands 2004   NA           NA  0.05; CO2
      251    Netherlands 2005   NA           NA  0.05; CO2
      252    Netherlands 2006   NA           NA  0.05; CO2
      253    Netherlands 2007   NA           NA  0.05; CO2
      254    Netherlands 2008   NA           NA  0.05; CO2
      255    Netherlands 2009   NA           NA  0.05; CO2
      256    Netherlands 2010   NA           NA  0.05; CO2
      257    Netherlands 2011   NA           NA  0.05; CO2
      258    Netherlands 2012   NA           NA  0.05; CO2
      259    Netherlands 2013   NA           NA  0.05; CO2
      260    Netherlands 2014   NA           NA  0.05; CO2
      261    Netherlands 2015   NA           NA  0.05; CO2
      262    Netherlands 2016   NA           NA  0.05; CO2
      263    Netherlands 2017   NA           NA  0.05; CO2
      264    Netherlands 2018   NA           NA  0.05; CO2
      265       Portugal 1995   NA           NA  0.05; CO2
      266       Portugal 1996   NA           NA  0.05; CO2
      267       Portugal 1997   NA           NA  0.05; CO2
      268       Portugal 1998   NA           NA  0.05; CO2
      269       Portugal 1999   NA           NA  0.05; CO2
      270       Portugal 2000   NA  0.114488548  0.05; CO2
      271       Portugal 2001   NA  0.114488548  0.05; CO2
      272       Portugal 2002   NA  0.114488548  0.05; CO2
      273       Portugal 2003   NA  0.114488548  0.05; CO2
      274       Portugal 2004   NA  0.114488548  0.05; CO2
      275       Portugal 2005   NA  0.114488548  0.05; CO2
      276       Portugal 2006   NA  0.114488548  0.05; CO2
      277       Portugal 2007   NA  0.114488548  0.05; CO2
      278       Portugal 2008   NA  0.114488548  0.05; CO2
      279       Portugal 2009   NA  0.114488548  0.05; CO2
      280       Portugal 2010   NA  0.114488548  0.05; CO2
      281       Portugal 2011   NA  0.114488548  0.05; CO2
      282       Portugal 2012   NA  0.114488548  0.05; CO2
      283       Portugal 2013   NA  0.114488548  0.05; CO2
      284       Portugal 2014   NA  0.114488548  0.05; CO2
      285       Portugal 2015   NA  0.114488548  0.05; CO2
      286       Portugal 2016   NA  0.114488548  0.05; CO2
      287       Portugal 2017   NA  0.114488548  0.05; CO2
      288       Portugal 2018   NA  0.114488548  0.05; CO2
      289          Spain 1995   NA           NA  0.05; CO2
      290          Spain 1996   NA           NA  0.05; CO2
      291          Spain 1997   NA           NA  0.05; CO2
      292          Spain 1998   NA           NA  0.05; CO2
      293          Spain 1999   NA           NA  0.05; CO2
      294          Spain 2000   NA           NA  0.05; CO2
      295          Spain 2001   NA  0.081748011  0.05; CO2
      296          Spain 2002   NA  0.081748011  0.05; CO2
      297          Spain 2003   NA  0.081748011  0.05; CO2
      298          Spain 2004   NA  0.081748011  0.05; CO2
      299          Spain 2005   NA  0.081748011  0.05; CO2
      300          Spain 2006   NA  0.081748011  0.05; CO2
      301          Spain 2007   NA  0.081748011  0.05; CO2
      302          Spain 2008   NA  0.081748011  0.05; CO2
      303          Spain 2009   NA  0.081748011  0.05; CO2
      304          Spain 2010   NA  0.081748011  0.05; CO2
      305          Spain 2011   NA  0.081748011  0.05; CO2
      306          Spain 2012   NA  0.081748011  0.05; CO2
      307          Spain 2013   NA  0.081748011  0.05; CO2
      308          Spain 2014   NA  0.081748011  0.05; CO2
      309          Spain 2015   NA  0.081748011  0.05; CO2
      310          Spain 2016   NA  0.081748011  0.05; CO2
      311          Spain 2017   NA  0.081748011  0.05; CO2
      312          Spain 2018   NA  0.081748011  0.05; CO2
      313         Sweden 1995   NA           NA  0.05; CO2
      314         Sweden 1996   NA           NA  0.05; CO2
      315         Sweden 1997   NA           NA  0.05; CO2
      316         Sweden 1998   NA           NA  0.05; CO2
      317         Sweden 1999   NA           NA  0.05; CO2
      318         Sweden 2000   NA           NA  0.05; CO2
      319         Sweden 2001   NA -0.094567781  0.05; CO2
      320         Sweden 2002   NA -0.094567781  0.05; CO2
      321         Sweden 2003   NA -0.094567781  0.05; CO2
      322         Sweden 2004   NA -0.094567781  0.05; CO2
      323         Sweden 2005   NA -0.094567781  0.05; CO2
      324         Sweden 2006   NA -0.094567781  0.05; CO2
      325         Sweden 2007   NA -0.094567781  0.05; CO2
      326         Sweden 2008   NA -0.094567781  0.05; CO2
      327         Sweden 2009   NA -0.094567781  0.05; CO2
      328         Sweden 2010   NA -0.094567781  0.05; CO2
      329         Sweden 2011   NA -0.094567781  0.05; CO2
      330         Sweden 2012   NA -0.094567781  0.05; CO2
      331         Sweden 2013   NA -0.094567781  0.05; CO2
      332         Sweden 2014   NA -0.094567781  0.05; CO2
      333         Sweden 2015   NA -0.094567781  0.05; CO2
      334         Sweden 2016   NA -0.094567781  0.05; CO2
      335         Sweden 2017   NA -0.094567781  0.05; CO2
      336         Sweden 2018   NA -0.094567781  0.05; CO2
      337  UnitedKingdom 1995   NA           NA  0.05; CO2
      338  UnitedKingdom 1996   NA           NA  0.05; CO2
      339  UnitedKingdom 1997   NA           NA  0.05; CO2
      340  UnitedKingdom 1998   NA           NA  0.05; CO2
      341  UnitedKingdom 1999   NA           NA  0.05; CO2
      342  UnitedKingdom 2000   NA           NA  0.05; CO2
      343  UnitedKingdom 2001   NA           NA  0.05; CO2
      344  UnitedKingdom 2002   NA           NA  0.05; CO2
      345  UnitedKingdom 2003   NA           NA  0.05; CO2
      346  UnitedKingdom 2004   NA           NA  0.05; CO2
      347  UnitedKingdom 2005   NA           NA  0.05; CO2
      348  UnitedKingdom 2006   NA           NA  0.05; CO2
      349  UnitedKingdom 2007   NA           NA  0.05; CO2
      350  UnitedKingdom 2008   NA           NA  0.05; CO2
      351  UnitedKingdom 2009   NA           NA  0.05; CO2
      352  UnitedKingdom 2010   NA           NA  0.05; CO2
      353  UnitedKingdom 2011   NA           NA  0.05; CO2
      354  UnitedKingdom 2012   NA           NA  0.05; CO2
      355  UnitedKingdom 2013   NA           NA  0.05; CO2
      356  UnitedKingdom 2014   NA  0.085855654  0.05; CO2
      357  UnitedKingdom 2015   NA  0.085855654  0.05; CO2
      358  UnitedKingdom 2016   NA  0.085855654  0.05; CO2
      359  UnitedKingdom 2017   NA  0.085855654  0.05; CO2
      360  UnitedKingdom 2018   NA  0.085855654  0.05; CO2
      361        Austria 1995   NA           NA  0.01; CO2
      362        Austria 1996   NA           NA  0.01; CO2
      363        Austria 1997   NA           NA  0.01; CO2
      364        Austria 1998   NA           NA  0.01; CO2
      365        Austria 1999   NA           NA  0.01; CO2
      366        Austria 2000   NA           NA  0.01; CO2
      367        Austria 2001   NA           NA  0.01; CO2
      368        Austria 2002   NA  0.145557453  0.01; CO2
      369        Austria 2003   NA  0.145557453  0.01; CO2
      370        Austria 2004   NA  0.145557453  0.01; CO2
      371        Austria 2005   NA  0.145557453  0.01; CO2
      372        Austria 2006   NA  0.145557453  0.01; CO2
      373        Austria 2007   NA  0.145557453  0.01; CO2
      374        Austria 2008   NA  0.145557453  0.01; CO2
      375        Austria 2009   NA  0.145557453  0.01; CO2
      376        Austria 2010   NA  0.145557453  0.01; CO2
      377        Austria 2011   NA  0.145557453  0.01; CO2
      378        Austria 2012   NA  0.145557453  0.01; CO2
      379        Austria 2013   NA  0.226584285  0.01; CO2
      380        Austria 2014   NA  0.226584285  0.01; CO2
      381        Austria 2015   NA  0.226584285  0.01; CO2
      382        Austria 2016   NA  0.226584285  0.01; CO2
      383        Austria 2017   NA  0.226584285  0.01; CO2
      384        Austria 2018   NA  0.226584285  0.01; CO2
      385        Belgium 1995   NA           NA  0.01; CO2
      386        Belgium 1996   NA           NA  0.01; CO2
      387        Belgium 1997   NA           NA  0.01; CO2
      388        Belgium 1998   NA           NA  0.01; CO2
      389        Belgium 1999   NA           NA  0.01; CO2
      390        Belgium 2000   NA           NA  0.01; CO2
      391        Belgium 2001   NA           NA  0.01; CO2
      392        Belgium 2002   NA           NA  0.01; CO2
      393        Belgium 2003   NA           NA  0.01; CO2
      394        Belgium 2004   NA           NA  0.01; CO2
      395        Belgium 2005   NA           NA  0.01; CO2
      396        Belgium 2006   NA           NA  0.01; CO2
      397        Belgium 2007   NA           NA  0.01; CO2
      398        Belgium 2008   NA           NA  0.01; CO2
      399        Belgium 2009   NA           NA  0.01; CO2
      400        Belgium 2010   NA           NA  0.01; CO2
      401        Belgium 2011   NA           NA  0.01; CO2
      402        Belgium 2012   NA           NA  0.01; CO2
      403        Belgium 2013   NA           NA  0.01; CO2
      404        Belgium 2014   NA           NA  0.01; CO2
      405        Belgium 2015   NA           NA  0.01; CO2
      406        Belgium 2016   NA           NA  0.01; CO2
      407        Belgium 2017   NA           NA  0.01; CO2
      408        Belgium 2018   NA           NA  0.01; CO2
      409        Denmark 1995   NA           NA  0.01; CO2
      410        Denmark 1996   NA           NA  0.01; CO2
      411        Denmark 1997   NA           NA  0.01; CO2
      412        Denmark 1998   NA           NA  0.01; CO2
      413        Denmark 1999   NA           NA  0.01; CO2
      414        Denmark 2000   NA           NA  0.01; CO2
      415        Denmark 2001   NA           NA  0.01; CO2
      416        Denmark 2002   NA           NA  0.01; CO2
      417        Denmark 2003   NA           NA  0.01; CO2
      418        Denmark 2004   NA           NA  0.01; CO2
      419        Denmark 2005   NA           NA  0.01; CO2
      420        Denmark 2006   NA           NA  0.01; CO2
      421        Denmark 2007   NA           NA  0.01; CO2
      422        Denmark 2008   NA           NA  0.01; CO2
      423        Denmark 2009   NA           NA  0.01; CO2
      424        Denmark 2010   NA           NA  0.01; CO2
      425        Denmark 2011   NA           NA  0.01; CO2
      426        Denmark 2012   NA           NA  0.01; CO2
      427        Denmark 2013   NA           NA  0.01; CO2
      428        Denmark 2014   NA           NA  0.01; CO2
      429        Denmark 2015   NA           NA  0.01; CO2
      430        Denmark 2016   NA           NA  0.01; CO2
      431        Denmark 2017   NA           NA  0.01; CO2
      432        Denmark 2018   NA           NA  0.01; CO2
      433        Finland 1995   NA           NA  0.01; CO2
      434        Finland 1996   NA           NA  0.01; CO2
      435        Finland 1997   NA           NA  0.01; CO2
      436        Finland 1998   NA           NA  0.01; CO2
      437        Finland 1999   NA           NA  0.01; CO2
      438        Finland 2000   NA -0.123332514  0.01; CO2
      439        Finland 2001   NA -0.123332514  0.01; CO2
      440        Finland 2002   NA -0.123332514  0.01; CO2
      441        Finland 2003   NA -0.123332514  0.01; CO2
      442        Finland 2004   NA -0.123332514  0.01; CO2
      443        Finland 2005   NA -0.123332514  0.01; CO2
      444        Finland 2006   NA -0.123332514  0.01; CO2
      445        Finland 2007   NA -0.123332514  0.01; CO2
      446        Finland 2008   NA -0.123332514  0.01; CO2
      447        Finland 2009   NA -0.123332514  0.01; CO2
      448        Finland 2010   NA -0.123332514  0.01; CO2
      449        Finland 2011   NA -0.123332514  0.01; CO2
      450        Finland 2012   NA -0.123332514  0.01; CO2
      451        Finland 2013   NA -0.123332514  0.01; CO2
      452        Finland 2014   NA -0.123332514  0.01; CO2
      453        Finland 2015   NA -0.123332514  0.01; CO2
      454        Finland 2016   NA -0.123332514  0.01; CO2
      455        Finland 2017   NA -0.123332514  0.01; CO2
      456        Finland 2018   NA -0.123332514  0.01; CO2
      457         France 1995   NA           NA  0.01; CO2
      458         France 1996   NA           NA  0.01; CO2
      459         France 1997   NA           NA  0.01; CO2
      460         France 1998   NA           NA  0.01; CO2
      461         France 1999   NA           NA  0.01; CO2
      462         France 2000   NA           NA  0.01; CO2
      463         France 2001   NA           NA  0.01; CO2
      464         France 2002   NA           NA  0.01; CO2
      465         France 2003   NA           NA  0.01; CO2
      466         France 2004   NA           NA  0.01; CO2
      467         France 2005   NA           NA  0.01; CO2
      468         France 2006   NA           NA  0.01; CO2
      469         France 2007   NA           NA  0.01; CO2
      470         France 2008   NA           NA  0.01; CO2
      471         France 2009   NA           NA  0.01; CO2
      472         France 2010   NA           NA  0.01; CO2
      473         France 2011   NA           NA  0.01; CO2
      474         France 2012   NA           NA  0.01; CO2
      475         France 2013   NA           NA  0.01; CO2
      476         France 2014   NA           NA  0.01; CO2
      477         France 2015   NA           NA  0.01; CO2
      478         France 2016   NA           NA  0.01; CO2
      479         France 2017   NA           NA  0.01; CO2
      480         France 2018   NA           NA  0.01; CO2
      481        Germany 1995   NA           NA  0.01; CO2
      482        Germany 1996   NA           NA  0.01; CO2
      483        Germany 1997   NA           NA  0.01; CO2
      484        Germany 1998   NA           NA  0.01; CO2
      485        Germany 1999   NA           NA  0.01; CO2
      486        Germany 2000   NA           NA  0.01; CO2
      487        Germany 2001   NA           NA  0.01; CO2
      488        Germany 2002   NA -0.130677429  0.01; CO2
      489        Germany 2003   NA -0.130677429  0.01; CO2
      490        Germany 2004   NA -0.130677429  0.01; CO2
      491        Germany 2005   NA -0.130677429  0.01; CO2
      492        Germany 2006   NA -0.130677429  0.01; CO2
      493        Germany 2007   NA -0.130677429  0.01; CO2
      494        Germany 2008   NA -0.130677429  0.01; CO2
      495        Germany 2009   NA -0.130677429  0.01; CO2
      496        Germany 2010   NA -0.130677429  0.01; CO2
      497        Germany 2011   NA -0.130677429  0.01; CO2
      498        Germany 2012   NA -0.130677429  0.01; CO2
      499        Germany 2013   NA  0.003700933  0.01; CO2
      500        Germany 2014   NA  0.003700933  0.01; CO2
      501        Germany 2015   NA  0.003700933  0.01; CO2
      502        Germany 2016   NA  0.003700933  0.01; CO2
      503        Germany 2017   NA  0.003700933  0.01; CO2
      504        Germany 2018   NA  0.003700933  0.01; CO2
      505         Greece 1995   NA           NA  0.01; CO2
      506         Greece 1996   NA           NA  0.01; CO2
      507         Greece 1997   NA           NA  0.01; CO2
      508         Greece 1998   NA           NA  0.01; CO2
      509         Greece 1999   NA           NA  0.01; CO2
      510         Greece 2000   NA           NA  0.01; CO2
      511         Greece 2001   NA           NA  0.01; CO2
      512         Greece 2002   NA           NA  0.01; CO2
      513         Greece 2003   NA           NA  0.01; CO2
      514         Greece 2004   NA           NA  0.01; CO2
      515         Greece 2005   NA           NA  0.01; CO2
      516         Greece 2006   NA           NA  0.01; CO2
      517         Greece 2007   NA           NA  0.01; CO2
      518         Greece 2008   NA           NA  0.01; CO2
      519         Greece 2009   NA           NA  0.01; CO2
      520         Greece 2010   NA           NA  0.01; CO2
      521         Greece 2011   NA           NA  0.01; CO2
      522         Greece 2012   NA           NA  0.01; CO2
      523         Greece 2013   NA           NA  0.01; CO2
      524         Greece 2014   NA           NA  0.01; CO2
      525         Greece 2015   NA           NA  0.01; CO2
      526         Greece 2016   NA           NA  0.01; CO2
      527         Greece 2017   NA           NA  0.01; CO2
      528         Greece 2018   NA           NA  0.01; CO2
      529        Ireland 1995   NA           NA  0.01; CO2
      530        Ireland 1996   NA           NA  0.01; CO2
      531        Ireland 1997   NA           NA  0.01; CO2
      532        Ireland 1998   NA  0.126551738  0.01; CO2
      533        Ireland 1999   NA  0.126551738  0.01; CO2
      534        Ireland 2000   NA  0.126551738  0.01; CO2
      535        Ireland 2001   NA  0.126551738  0.01; CO2
      536        Ireland 2002   NA  0.126551738  0.01; CO2
      537        Ireland 2003   NA  0.126551738  0.01; CO2
      538        Ireland 2004   NA  0.126551738  0.01; CO2
      539        Ireland 2005   NA  0.126551738  0.01; CO2
      540        Ireland 2006   NA  0.126551738  0.01; CO2
      541        Ireland 2007   NA  0.126551738  0.01; CO2
      542        Ireland 2008   NA  0.126551738  0.01; CO2
      543        Ireland 2009   NA  0.126551738  0.01; CO2
      544        Ireland 2010   NA  0.126551738  0.01; CO2
      545        Ireland 2011   NA  0.126551738  0.01; CO2
      546        Ireland 2012   NA  0.126551738  0.01; CO2
      547        Ireland 2013   NA  0.126551738  0.01; CO2
      548        Ireland 2014   NA  0.126551738  0.01; CO2
      549        Ireland 2015   NA -0.065630096  0.01; CO2
      550        Ireland 2016   NA -0.065630096  0.01; CO2
      551        Ireland 2017   NA -0.065630096  0.01; CO2
      552        Ireland 2018   NA -0.065630096  0.01; CO2
      553          Italy 1995   NA           NA  0.01; CO2
      554          Italy 1996   NA           NA  0.01; CO2
      555          Italy 1997   NA           NA  0.01; CO2
      556          Italy 1998   NA           NA  0.01; CO2
      557          Italy 1999   NA           NA  0.01; CO2
      558          Italy 2000   NA           NA  0.01; CO2
      559          Italy 2001   NA           NA  0.01; CO2
      560          Italy 2002   NA           NA  0.01; CO2
      561          Italy 2003   NA           NA  0.01; CO2
      562          Italy 2004   NA           NA  0.01; CO2
      563          Italy 2005   NA           NA  0.01; CO2
      564          Italy 2006   NA           NA  0.01; CO2
      565          Italy 2007   NA           NA  0.01; CO2
      566          Italy 2008   NA           NA  0.01; CO2
      567          Italy 2009   NA           NA  0.01; CO2
      568          Italy 2010   NA           NA  0.01; CO2
      569          Italy 2011   NA           NA  0.01; CO2
      570          Italy 2012   NA           NA  0.01; CO2
      571          Italy 2013   NA           NA  0.01; CO2
      572          Italy 2014   NA           NA  0.01; CO2
      573          Italy 2015   NA           NA  0.01; CO2
      574          Italy 2016   NA           NA  0.01; CO2
      575          Italy 2017   NA           NA  0.01; CO2
      576          Italy 2018   NA           NA  0.01; CO2
      577     Luxembourg 1995   NA           NA  0.01; CO2
      578     Luxembourg 1996   NA           NA  0.01; CO2
      579     Luxembourg 1997   NA           NA  0.01; CO2
      580     Luxembourg 1998   NA           NA  0.01; CO2
      581     Luxembourg 1999   NA           NA  0.01; CO2
      582     Luxembourg 2000   NA           NA  0.01; CO2
      583     Luxembourg 2001   NA           NA  0.01; CO2
      584     Luxembourg 2002   NA           NA  0.01; CO2
      585     Luxembourg 2003   NA  0.105495661  0.01; CO2
      586     Luxembourg 2004   NA  0.105495661  0.01; CO2
      587     Luxembourg 2005   NA  0.105495661  0.01; CO2
      588     Luxembourg 2006   NA  0.105495661  0.01; CO2
      589     Luxembourg 2007   NA  0.105495661  0.01; CO2
      590     Luxembourg 2008   NA  0.105495661  0.01; CO2
      591     Luxembourg 2009   NA  0.105495661  0.01; CO2
      592     Luxembourg 2010   NA  0.105495661  0.01; CO2
      593     Luxembourg 2011   NA  0.105495661  0.01; CO2
      594     Luxembourg 2012   NA  0.105495661  0.01; CO2
      595     Luxembourg 2013   NA  0.105495661  0.01; CO2
      596     Luxembourg 2014   NA  0.105495661  0.01; CO2
      597     Luxembourg 2015   NA  0.105495661  0.01; CO2
      598     Luxembourg 2016   NA  0.105495661  0.01; CO2
      599     Luxembourg 2017   NA  0.105495661  0.01; CO2
      600     Luxembourg 2018   NA  0.105495661  0.01; CO2
      601    Netherlands 1995   NA           NA  0.01; CO2
      602    Netherlands 1996   NA           NA  0.01; CO2
      603    Netherlands 1997   NA           NA  0.01; CO2
      604    Netherlands 1998   NA           NA  0.01; CO2
      605    Netherlands 1999   NA           NA  0.01; CO2
      606    Netherlands 2000   NA           NA  0.01; CO2
      607    Netherlands 2001   NA           NA  0.01; CO2
      608    Netherlands 2002   NA           NA  0.01; CO2
      609    Netherlands 2003   NA           NA  0.01; CO2
      610    Netherlands 2004   NA           NA  0.01; CO2
      611    Netherlands 2005   NA           NA  0.01; CO2
      612    Netherlands 2006   NA           NA  0.01; CO2
      613    Netherlands 2007   NA           NA  0.01; CO2
      614    Netherlands 2008   NA           NA  0.01; CO2
      615    Netherlands 2009   NA           NA  0.01; CO2
      616    Netherlands 2010   NA           NA  0.01; CO2
      617    Netherlands 2011   NA           NA  0.01; CO2
      618    Netherlands 2012   NA           NA  0.01; CO2
      619    Netherlands 2013   NA           NA  0.01; CO2
      620    Netherlands 2014   NA           NA  0.01; CO2
      621    Netherlands 2015   NA           NA  0.01; CO2
      622    Netherlands 2016   NA           NA  0.01; CO2
      623    Netherlands 2017   NA           NA  0.01; CO2
      624    Netherlands 2018   NA           NA  0.01; CO2
      625       Portugal 1995   NA           NA  0.01; CO2
      626       Portugal 1996   NA           NA  0.01; CO2
      627       Portugal 1997   NA           NA  0.01; CO2
      628       Portugal 1998   NA           NA  0.01; CO2
      629       Portugal 1999   NA           NA  0.01; CO2
      630       Portugal 2000   NA  0.105614112  0.01; CO2
      631       Portugal 2001   NA  0.105614112  0.01; CO2
      632       Portugal 2002   NA  0.105614112  0.01; CO2
      633       Portugal 2003   NA  0.105614112  0.01; CO2
      634       Portugal 2004   NA  0.105614112  0.01; CO2
      635       Portugal 2005   NA  0.105614112  0.01; CO2
      636       Portugal 2006   NA  0.105614112  0.01; CO2
      637       Portugal 2007   NA  0.105614112  0.01; CO2
      638       Portugal 2008   NA  0.105614112  0.01; CO2
      639       Portugal 2009   NA  0.105614112  0.01; CO2
      640       Portugal 2010   NA  0.105614112  0.01; CO2
      641       Portugal 2011   NA  0.105614112  0.01; CO2
      642       Portugal 2012   NA  0.105614112  0.01; CO2
      643       Portugal 2013   NA  0.105614112  0.01; CO2
      644       Portugal 2014   NA  0.105614112  0.01; CO2
      645       Portugal 2015   NA  0.105614112  0.01; CO2
      646       Portugal 2016   NA  0.105614112  0.01; CO2
      647       Portugal 2017   NA  0.105614112  0.01; CO2
      648       Portugal 2018   NA  0.105614112  0.01; CO2
      649          Spain 1995   NA           NA  0.01; CO2
      650          Spain 1996   NA           NA  0.01; CO2
      651          Spain 1997   NA           NA  0.01; CO2
      652          Spain 1998   NA           NA  0.01; CO2
      653          Spain 1999   NA           NA  0.01; CO2
      654          Spain 2000   NA           NA  0.01; CO2
      655          Spain 2001   NA  0.129710968  0.01; CO2
      656          Spain 2002   NA  0.129710968  0.01; CO2
      657          Spain 2003   NA  0.129710968  0.01; CO2
      658          Spain 2004   NA  0.129710968  0.01; CO2
      659          Spain 2005   NA  0.129710968  0.01; CO2
      660          Spain 2006   NA  0.129710968  0.01; CO2
      661          Spain 2007   NA  0.129710968  0.01; CO2
      662          Spain 2008   NA  0.129710968  0.01; CO2
      663          Spain 2009   NA  0.129710968  0.01; CO2
      664          Spain 2010   NA  0.129710968  0.01; CO2
      665          Spain 2011   NA  0.129710968  0.01; CO2
      666          Spain 2012   NA  0.129710968  0.01; CO2
      667          Spain 2013   NA  0.129710968  0.01; CO2
      668          Spain 2014   NA  0.129710968  0.01; CO2
      669          Spain 2015   NA  0.129710968  0.01; CO2
      670          Spain 2016   NA  0.129710968  0.01; CO2
      671          Spain 2017   NA  0.129710968  0.01; CO2
      672          Spain 2018   NA  0.129710968  0.01; CO2
      673         Sweden 1995   NA           NA  0.01; CO2
      674         Sweden 1996   NA           NA  0.01; CO2
      675         Sweden 1997   NA           NA  0.01; CO2
      676         Sweden 1998   NA           NA  0.01; CO2
      677         Sweden 1999   NA           NA  0.01; CO2
      678         Sweden 2000   NA           NA  0.01; CO2
      679         Sweden 2001   NA -0.102652685  0.01; CO2
      680         Sweden 2002   NA -0.102652685  0.01; CO2
      681         Sweden 2003   NA -0.102652685  0.01; CO2
      682         Sweden 2004   NA -0.102652685  0.01; CO2
      683         Sweden 2005   NA -0.102652685  0.01; CO2
      684         Sweden 2006   NA -0.102652685  0.01; CO2
      685         Sweden 2007   NA -0.102652685  0.01; CO2
      686         Sweden 2008   NA -0.102652685  0.01; CO2
      687         Sweden 2009   NA -0.102652685  0.01; CO2
      688         Sweden 2010   NA -0.102652685  0.01; CO2
      689         Sweden 2011   NA -0.102652685  0.01; CO2
      690         Sweden 2012   NA -0.102652685  0.01; CO2
      691         Sweden 2013   NA -0.102652685  0.01; CO2
      692         Sweden 2014   NA -0.102652685  0.01; CO2
      693         Sweden 2015   NA -0.102652685  0.01; CO2
      694         Sweden 2016   NA -0.102652685  0.01; CO2
      695         Sweden 2017   NA -0.102652685  0.01; CO2
      696         Sweden 2018   NA -0.102652685  0.01; CO2
      697  UnitedKingdom 1995   NA           NA  0.01; CO2
      698  UnitedKingdom 1996   NA           NA  0.01; CO2
      699  UnitedKingdom 1997   NA           NA  0.01; CO2
      700  UnitedKingdom 1998   NA           NA  0.01; CO2
      701  UnitedKingdom 1999   NA           NA  0.01; CO2
      702  UnitedKingdom 2000   NA           NA  0.01; CO2
      703  UnitedKingdom 2001   NA           NA  0.01; CO2
      704  UnitedKingdom 2002   NA           NA  0.01; CO2
      705  UnitedKingdom 2003   NA           NA  0.01; CO2
      706  UnitedKingdom 2004   NA           NA  0.01; CO2
      707  UnitedKingdom 2005   NA           NA  0.01; CO2
      708  UnitedKingdom 2006   NA           NA  0.01; CO2
      709  UnitedKingdom 2007   NA           NA  0.01; CO2
      710  UnitedKingdom 2008   NA           NA  0.01; CO2
      711  UnitedKingdom 2009   NA           NA  0.01; CO2
      712  UnitedKingdom 2010   NA           NA  0.01; CO2
      713  UnitedKingdom 2011   NA           NA  0.01; CO2
      714  UnitedKingdom 2012   NA           NA  0.01; CO2
      715  UnitedKingdom 2013   NA           NA  0.01; CO2
      716  UnitedKingdom 2014   NA           NA  0.01; CO2
      717  UnitedKingdom 2015   NA           NA  0.01; CO2
      718  UnitedKingdom 2016   NA           NA  0.01; CO2
      719  UnitedKingdom 2017   NA           NA  0.01; CO2
      720  UnitedKingdom 2018   NA           NA  0.01; CO2
      721        Austria 1995   NA           NA 0.001; CO2
      722        Austria 1996   NA           NA 0.001; CO2
      723        Austria 1997   NA           NA 0.001; CO2
      724        Austria 1998   NA           NA 0.001; CO2
      725        Austria 1999   NA           NA 0.001; CO2
      726        Austria 2000   NA           NA 0.001; CO2
      727        Austria 2001   NA  0.140438233 0.001; CO2
      728        Austria 2002   NA  0.140438233 0.001; CO2
      729        Austria 2003   NA  0.140438233 0.001; CO2
      730        Austria 2004   NA  0.140438233 0.001; CO2
      731        Austria 2005   NA  0.140438233 0.001; CO2
      732        Austria 2006   NA  0.140438233 0.001; CO2
      733        Austria 2007   NA  0.140438233 0.001; CO2
      734        Austria 2008   NA  0.140438233 0.001; CO2
      735        Austria 2009   NA  0.140438233 0.001; CO2
      736        Austria 2010   NA  0.140438233 0.001; CO2
      737        Austria 2011   NA  0.140438233 0.001; CO2
      738        Austria 2012   NA  0.140438233 0.001; CO2
      739        Austria 2013   NA  0.223492675 0.001; CO2
      740        Austria 2014   NA  0.223492675 0.001; CO2
      741        Austria 2015   NA  0.223492675 0.001; CO2
      742        Austria 2016   NA  0.223492675 0.001; CO2
      743        Austria 2017   NA  0.223492675 0.001; CO2
      744        Austria 2018   NA  0.223492675 0.001; CO2
      745        Belgium 1995   NA           NA 0.001; CO2
      746        Belgium 1996   NA           NA 0.001; CO2
      747        Belgium 1997   NA           NA 0.001; CO2
      748        Belgium 1998   NA           NA 0.001; CO2
      749        Belgium 1999   NA           NA 0.001; CO2
      750        Belgium 2000   NA           NA 0.001; CO2
      751        Belgium 2001   NA           NA 0.001; CO2
      752        Belgium 2002   NA           NA 0.001; CO2
      753        Belgium 2003   NA           NA 0.001; CO2
      754        Belgium 2004   NA           NA 0.001; CO2
      755        Belgium 2005   NA           NA 0.001; CO2
      756        Belgium 2006   NA           NA 0.001; CO2
      757        Belgium 2007   NA           NA 0.001; CO2
      758        Belgium 2008   NA           NA 0.001; CO2
      759        Belgium 2009   NA           NA 0.001; CO2
      760        Belgium 2010   NA           NA 0.001; CO2
      761        Belgium 2011   NA           NA 0.001; CO2
      762        Belgium 2012   NA           NA 0.001; CO2
      763        Belgium 2013   NA           NA 0.001; CO2
      764        Belgium 2014   NA           NA 0.001; CO2
      765        Belgium 2015   NA           NA 0.001; CO2
      766        Belgium 2016   NA           NA 0.001; CO2
      767        Belgium 2017   NA           NA 0.001; CO2
      768        Belgium 2018   NA           NA 0.001; CO2
      769        Denmark 1995   NA           NA 0.001; CO2
      770        Denmark 1996   NA           NA 0.001; CO2
      771        Denmark 1997   NA           NA 0.001; CO2
      772        Denmark 1998   NA           NA 0.001; CO2
      773        Denmark 1999   NA           NA 0.001; CO2
      774        Denmark 2000   NA           NA 0.001; CO2
      775        Denmark 2001   NA           NA 0.001; CO2
      776        Denmark 2002   NA           NA 0.001; CO2
      777        Denmark 2003   NA           NA 0.001; CO2
      778        Denmark 2004   NA           NA 0.001; CO2
      779        Denmark 2005   NA           NA 0.001; CO2
      780        Denmark 2006   NA           NA 0.001; CO2
      781        Denmark 2007   NA           NA 0.001; CO2
      782        Denmark 2008   NA           NA 0.001; CO2
      783        Denmark 2009   NA           NA 0.001; CO2
      784        Denmark 2010   NA           NA 0.001; CO2
      785        Denmark 2011   NA           NA 0.001; CO2
      786        Denmark 2012   NA           NA 0.001; CO2
      787        Denmark 2013   NA           NA 0.001; CO2
      788        Denmark 2014   NA           NA 0.001; CO2
      789        Denmark 2015   NA           NA 0.001; CO2
      790        Denmark 2016   NA           NA 0.001; CO2
      791        Denmark 2017   NA           NA 0.001; CO2
      792        Denmark 2018   NA           NA 0.001; CO2
      793        Finland 1995   NA           NA 0.001; CO2
      794        Finland 1996   NA           NA 0.001; CO2
      795        Finland 1997   NA           NA 0.001; CO2
      796        Finland 1998   NA           NA 0.001; CO2
      797        Finland 1999   NA           NA 0.001; CO2
      798        Finland 2000   NA -0.127685560 0.001; CO2
      799        Finland 2001   NA -0.127685560 0.001; CO2
      800        Finland 2002   NA -0.127685560 0.001; CO2
      801        Finland 2003   NA -0.127685560 0.001; CO2
      802        Finland 2004   NA -0.127685560 0.001; CO2
      803        Finland 2005   NA -0.127685560 0.001; CO2
      804        Finland 2006   NA -0.127685560 0.001; CO2
      805        Finland 2007   NA -0.127685560 0.001; CO2
      806        Finland 2008   NA -0.127685560 0.001; CO2
      807        Finland 2009   NA -0.127685560 0.001; CO2
      808        Finland 2010   NA -0.127685560 0.001; CO2
      809        Finland 2011   NA -0.127685560 0.001; CO2
      810        Finland 2012   NA -0.127685560 0.001; CO2
      811        Finland 2013   NA -0.127685560 0.001; CO2
      812        Finland 2014   NA -0.127685560 0.001; CO2
      813        Finland 2015   NA -0.127685560 0.001; CO2
      814        Finland 2016   NA -0.127685560 0.001; CO2
      815        Finland 2017   NA -0.127685560 0.001; CO2
      816        Finland 2018   NA -0.127685560 0.001; CO2
      817         France 1995   NA           NA 0.001; CO2
      818         France 1996   NA           NA 0.001; CO2
      819         France 1997   NA           NA 0.001; CO2
      820         France 1998   NA           NA 0.001; CO2
      821         France 1999   NA           NA 0.001; CO2
      822         France 2000   NA           NA 0.001; CO2
      823         France 2001   NA           NA 0.001; CO2
      824         France 2002   NA           NA 0.001; CO2
      825         France 2003   NA           NA 0.001; CO2
      826         France 2004   NA           NA 0.001; CO2
      827         France 2005   NA           NA 0.001; CO2
      828         France 2006   NA           NA 0.001; CO2
      829         France 2007   NA           NA 0.001; CO2
      830         France 2008   NA           NA 0.001; CO2
      831         France 2009   NA           NA 0.001; CO2
      832         France 2010   NA           NA 0.001; CO2
      833         France 2011   NA           NA 0.001; CO2
      834         France 2012   NA           NA 0.001; CO2
      835         France 2013   NA           NA 0.001; CO2
      836         France 2014   NA           NA 0.001; CO2
      837         France 2015   NA           NA 0.001; CO2
      838         France 2016   NA           NA 0.001; CO2
      839         France 2017   NA           NA 0.001; CO2
      840         France 2018   NA           NA 0.001; CO2
      841        Germany 1995   NA           NA 0.001; CO2
      842        Germany 1996   NA           NA 0.001; CO2
      843        Germany 1997   NA           NA 0.001; CO2
      844        Germany 1998   NA           NA 0.001; CO2
      845        Germany 1999   NA           NA 0.001; CO2
      846        Germany 2000   NA           NA 0.001; CO2
      847        Germany 2001   NA           NA 0.001; CO2
      848        Germany 2002   NA -0.108282785 0.001; CO2
      849        Germany 2003   NA -0.108282785 0.001; CO2
      850        Germany 2004   NA -0.108282785 0.001; CO2
      851        Germany 2005   NA -0.108282785 0.001; CO2
      852        Germany 2006   NA -0.108282785 0.001; CO2
      853        Germany 2007   NA -0.108282785 0.001; CO2
      854        Germany 2008   NA -0.108282785 0.001; CO2
      855        Germany 2009   NA -0.108282785 0.001; CO2
      856        Germany 2010   NA -0.108282785 0.001; CO2
      857        Germany 2011   NA -0.108282785 0.001; CO2
      858        Germany 2012   NA -0.108282785 0.001; CO2
      859        Germany 2013   NA -0.108282785 0.001; CO2
      860        Germany 2014   NA -0.108282785 0.001; CO2
      861        Germany 2015   NA -0.108282785 0.001; CO2
      862        Germany 2016   NA -0.108282785 0.001; CO2
      863        Germany 2017   NA -0.108282785 0.001; CO2
      864        Germany 2018   NA -0.108282785 0.001; CO2
      865         Greece 1995   NA           NA 0.001; CO2
      866         Greece 1996   NA           NA 0.001; CO2
      867         Greece 1997   NA           NA 0.001; CO2
      868         Greece 1998   NA           NA 0.001; CO2
      869         Greece 1999   NA           NA 0.001; CO2
      870         Greece 2000   NA           NA 0.001; CO2
      871         Greece 2001   NA           NA 0.001; CO2
      872         Greece 2002   NA           NA 0.001; CO2
      873         Greece 2003   NA           NA 0.001; CO2
      874         Greece 2004   NA           NA 0.001; CO2
      875         Greece 2005   NA           NA 0.001; CO2
      876         Greece 2006   NA           NA 0.001; CO2
      877         Greece 2007   NA           NA 0.001; CO2
      878         Greece 2008   NA           NA 0.001; CO2
      879         Greece 2009   NA           NA 0.001; CO2
      880         Greece 2010   NA           NA 0.001; CO2
      881         Greece 2011   NA           NA 0.001; CO2
      882         Greece 2012   NA           NA 0.001; CO2
      883         Greece 2013   NA           NA 0.001; CO2
      884         Greece 2014   NA           NA 0.001; CO2
      885         Greece 2015   NA           NA 0.001; CO2
      886         Greece 2016   NA           NA 0.001; CO2
      887         Greece 2017   NA           NA 0.001; CO2
      888         Greece 2018   NA           NA 0.001; CO2
      889        Ireland 1995   NA           NA 0.001; CO2
      890        Ireland 1996   NA           NA 0.001; CO2
      891        Ireland 1997   NA           NA 0.001; CO2
      892        Ireland 1998   NA  0.170465313 0.001; CO2
      893        Ireland 1999   NA  0.170465313 0.001; CO2
      894        Ireland 2000   NA  0.170465313 0.001; CO2
      895        Ireland 2001   NA  0.170465313 0.001; CO2
      896        Ireland 2002   NA  0.170465313 0.001; CO2
      897        Ireland 2003   NA  0.170465313 0.001; CO2
      898        Ireland 2004   NA  0.170465313 0.001; CO2
      899        Ireland 2005   NA  0.170465313 0.001; CO2
      900        Ireland 2006   NA  0.170465313 0.001; CO2
      901        Ireland 2007   NA  0.170465313 0.001; CO2
      902        Ireland 2008   NA  0.170465313 0.001; CO2
      903        Ireland 2009   NA  0.170465313 0.001; CO2
      904        Ireland 2010   NA  0.170465313 0.001; CO2
      905        Ireland 2011   NA  0.043512618 0.001; CO2
      906        Ireland 2012   NA  0.043512618 0.001; CO2
      907        Ireland 2013   NA  0.043512618 0.001; CO2
      908        Ireland 2014   NA  0.043512618 0.001; CO2
      909        Ireland 2015   NA  0.043512618 0.001; CO2
      910        Ireland 2016   NA  0.043512618 0.001; CO2
      911        Ireland 2017   NA  0.043512618 0.001; CO2
      912        Ireland 2018   NA  0.043512618 0.001; CO2
      913          Italy 1995   NA           NA 0.001; CO2
      914          Italy 1996   NA           NA 0.001; CO2
      915          Italy 1997   NA           NA 0.001; CO2
      916          Italy 1998   NA           NA 0.001; CO2
      917          Italy 1999   NA           NA 0.001; CO2
      918          Italy 2000   NA           NA 0.001; CO2
      919          Italy 2001   NA           NA 0.001; CO2
      920          Italy 2002   NA           NA 0.001; CO2
      921          Italy 2003   NA           NA 0.001; CO2
      922          Italy 2004   NA           NA 0.001; CO2
      923          Italy 2005   NA           NA 0.001; CO2
      924          Italy 2006   NA           NA 0.001; CO2
      925          Italy 2007   NA           NA 0.001; CO2
      926          Italy 2008   NA           NA 0.001; CO2
      927          Italy 2009   NA           NA 0.001; CO2
      928          Italy 2010   NA           NA 0.001; CO2
      929          Italy 2011   NA           NA 0.001; CO2
      930          Italy 2012   NA           NA 0.001; CO2
      931          Italy 2013   NA           NA 0.001; CO2
      932          Italy 2014   NA           NA 0.001; CO2
      933          Italy 2015   NA           NA 0.001; CO2
      934          Italy 2016   NA           NA 0.001; CO2
      935          Italy 2017   NA           NA 0.001; CO2
      936          Italy 2018   NA           NA 0.001; CO2
      937     Luxembourg 1995   NA           NA 0.001; CO2
      938     Luxembourg 1996   NA           NA 0.001; CO2
      939     Luxembourg 1997   NA           NA 0.001; CO2
      940     Luxembourg 1998   NA           NA 0.001; CO2
      941     Luxembourg 1999   NA           NA 0.001; CO2
      942     Luxembourg 2000   NA           NA 0.001; CO2
      943     Luxembourg 2001   NA           NA 0.001; CO2
      944     Luxembourg 2002   NA           NA 0.001; CO2
      945     Luxembourg 2003   NA  0.146722100 0.001; CO2
      946     Luxembourg 2004   NA  0.146722100 0.001; CO2
      947     Luxembourg 2005   NA  0.146722100 0.001; CO2
      948     Luxembourg 2006   NA  0.146722100 0.001; CO2
      949     Luxembourg 2007   NA  0.146722100 0.001; CO2
      950     Luxembourg 2008   NA  0.146722100 0.001; CO2
      951     Luxembourg 2009   NA  0.146722100 0.001; CO2
      952     Luxembourg 2010   NA  0.146722100 0.001; CO2
      953     Luxembourg 2011   NA  0.146722100 0.001; CO2
      954     Luxembourg 2012   NA  0.146722100 0.001; CO2
      955     Luxembourg 2013   NA  0.146722100 0.001; CO2
      956     Luxembourg 2014   NA  0.146722100 0.001; CO2
      957     Luxembourg 2015   NA -0.067298252 0.001; CO2
      958     Luxembourg 2016   NA -0.067298252 0.001; CO2
      959     Luxembourg 2017   NA -0.067298252 0.001; CO2
      960     Luxembourg 2018   NA -0.067298252 0.001; CO2
      961    Netherlands 1995   NA           NA 0.001; CO2
      962    Netherlands 1996   NA           NA 0.001; CO2
      963    Netherlands 1997   NA           NA 0.001; CO2
      964    Netherlands 1998   NA           NA 0.001; CO2
      965    Netherlands 1999   NA           NA 0.001; CO2
      966    Netherlands 2000   NA           NA 0.001; CO2
      967    Netherlands 2001   NA           NA 0.001; CO2
      968    Netherlands 2002   NA           NA 0.001; CO2
      969    Netherlands 2003   NA           NA 0.001; CO2
      970    Netherlands 2004   NA           NA 0.001; CO2
      971    Netherlands 2005   NA           NA 0.001; CO2
      972    Netherlands 2006   NA           NA 0.001; CO2
      973    Netherlands 2007   NA           NA 0.001; CO2
      974    Netherlands 2008   NA           NA 0.001; CO2
      975    Netherlands 2009   NA           NA 0.001; CO2
      976    Netherlands 2010   NA           NA 0.001; CO2
      977    Netherlands 2011   NA           NA 0.001; CO2
      978    Netherlands 2012   NA           NA 0.001; CO2
      979    Netherlands 2013   NA           NA 0.001; CO2
      980    Netherlands 2014   NA           NA 0.001; CO2
      981    Netherlands 2015   NA           NA 0.001; CO2
      982    Netherlands 2016   NA           NA 0.001; CO2
      983    Netherlands 2017   NA           NA 0.001; CO2
      984    Netherlands 2018   NA           NA 0.001; CO2
      985       Portugal 1995   NA           NA 0.001; CO2
      986       Portugal 1996   NA           NA 0.001; CO2
      987       Portugal 1997   NA           NA 0.001; CO2
      988       Portugal 1998   NA           NA 0.001; CO2
      989       Portugal 1999   NA           NA 0.001; CO2
      990       Portugal 2000   NA           NA 0.001; CO2
      991       Portugal 2001   NA           NA 0.001; CO2
      992       Portugal 2002   NA           NA 0.001; CO2
      993       Portugal 2003   NA           NA 0.001; CO2
      994       Portugal 2004   NA           NA 0.001; CO2
      995       Portugal 2005   NA           NA 0.001; CO2
      996       Portugal 2006   NA           NA 0.001; CO2
      997       Portugal 2007   NA           NA 0.001; CO2
      998       Portugal 2008   NA           NA 0.001; CO2
      999       Portugal 2009   NA           NA 0.001; CO2
      1000      Portugal 2010   NA           NA 0.001; CO2
      1001      Portugal 2011   NA           NA 0.001; CO2
      1002      Portugal 2012   NA           NA 0.001; CO2
      1003      Portugal 2013   NA           NA 0.001; CO2
      1004      Portugal 2014   NA           NA 0.001; CO2
      1005      Portugal 2015   NA           NA 0.001; CO2
      1006      Portugal 2016   NA           NA 0.001; CO2
      1007      Portugal 2017   NA           NA 0.001; CO2
      1008      Portugal 2018   NA           NA 0.001; CO2
      1009         Spain 1995   NA           NA 0.001; CO2
      1010         Spain 1996   NA           NA 0.001; CO2
      1011         Spain 1997   NA           NA 0.001; CO2
      1012         Spain 1998   NA           NA 0.001; CO2
      1013         Spain 1999   NA  0.149017641 0.001; CO2
      1014         Spain 2000   NA  0.149017641 0.001; CO2
      1015         Spain 2001   NA  0.149017641 0.001; CO2
      1016         Spain 2002   NA  0.149017641 0.001; CO2
      1017         Spain 2003   NA  0.149017641 0.001; CO2
      1018         Spain 2004   NA  0.149017641 0.001; CO2
      1019         Spain 2005   NA  0.149017641 0.001; CO2
      1020         Spain 2006   NA  0.149017641 0.001; CO2
      1021         Spain 2007   NA  0.149017641 0.001; CO2
      1022         Spain 2008   NA  0.149017641 0.001; CO2
      1023         Spain 2009   NA  0.149017641 0.001; CO2
      1024         Spain 2010   NA  0.149017641 0.001; CO2
      1025         Spain 2011   NA  0.149017641 0.001; CO2
      1026         Spain 2012   NA  0.149017641 0.001; CO2
      1027         Spain 2013   NA  0.149017641 0.001; CO2
      1028         Spain 2014   NA  0.149017641 0.001; CO2
      1029         Spain 2015   NA  0.149017641 0.001; CO2
      1030         Spain 2016   NA  0.149017641 0.001; CO2
      1031         Spain 2017   NA  0.149017641 0.001; CO2
      1032         Spain 2018   NA  0.149017641 0.001; CO2
      1033        Sweden 1995   NA           NA 0.001; CO2
      1034        Sweden 1996   NA           NA 0.001; CO2
      1035        Sweden 1997   NA           NA 0.001; CO2
      1036        Sweden 1998   NA           NA 0.001; CO2
      1037        Sweden 1999   NA           NA 0.001; CO2
      1038        Sweden 2000   NA           NA 0.001; CO2
      1039        Sweden 2001   NA -0.109615780 0.001; CO2
      1040        Sweden 2002   NA -0.109615780 0.001; CO2
      1041        Sweden 2003   NA -0.109615780 0.001; CO2
      1042        Sweden 2004   NA -0.109615780 0.001; CO2
      1043        Sweden 2005   NA -0.109615780 0.001; CO2
      1044        Sweden 2006   NA -0.109615780 0.001; CO2
      1045        Sweden 2007   NA -0.109615780 0.001; CO2
      1046        Sweden 2008   NA -0.109615780 0.001; CO2
      1047        Sweden 2009   NA -0.109615780 0.001; CO2
      1048        Sweden 2010   NA -0.109615780 0.001; CO2
      1049        Sweden 2011   NA -0.109615780 0.001; CO2
      1050        Sweden 2012   NA -0.109615780 0.001; CO2
      1051        Sweden 2013   NA -0.109615780 0.001; CO2
      1052        Sweden 2014   NA -0.109615780 0.001; CO2
      1053        Sweden 2015   NA -0.109615780 0.001; CO2
      1054        Sweden 2016   NA -0.109615780 0.001; CO2
      1055        Sweden 2017   NA -0.109615780 0.001; CO2
      1056        Sweden 2018   NA -0.109615780 0.001; CO2
      1057 UnitedKingdom 1995   NA           NA 0.001; CO2
      1058 UnitedKingdom 1996   NA           NA 0.001; CO2
      1059 UnitedKingdom 1997   NA           NA 0.001; CO2
      1060 UnitedKingdom 1998   NA           NA 0.001; CO2
      1061 UnitedKingdom 1999   NA           NA 0.001; CO2
      1062 UnitedKingdom 2000   NA           NA 0.001; CO2
      1063 UnitedKingdom 2001   NA           NA 0.001; CO2
      1064 UnitedKingdom 2002   NA           NA 0.001; CO2
      1065 UnitedKingdom 2003   NA           NA 0.001; CO2
      1066 UnitedKingdom 2004   NA           NA 0.001; CO2
      1067 UnitedKingdom 2005   NA           NA 0.001; CO2
      1068 UnitedKingdom 2006   NA           NA 0.001; CO2
      1069 UnitedKingdom 2007   NA           NA 0.001; CO2
      1070 UnitedKingdom 2008   NA           NA 0.001; CO2
      1071 UnitedKingdom 2009   NA           NA 0.001; CO2
      1072 UnitedKingdom 2010   NA           NA 0.001; CO2
      1073 UnitedKingdom 2011   NA           NA 0.001; CO2
      1074 UnitedKingdom 2012   NA           NA 0.001; CO2
      1075 UnitedKingdom 2013   NA           NA 0.001; CO2
      1076 UnitedKingdom 2014   NA           NA 0.001; CO2
      1077 UnitedKingdom 2015   NA           NA 0.001; CO2
      1078 UnitedKingdom 2016   NA           NA 0.001; CO2
      1079 UnitedKingdom 2017   NA           NA 0.001; CO2
      1080 UnitedKingdom 2018   NA           NA 0.001; CO2
      
      $layers
      $layers[[1]]
      mapping: fill = ~.data$effect 
      geom_tile: linejoin = mitre, na.rm = TRUE
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
          train_df: function
          transform_df: function
          super:  <ggproto object: Class ScalesList, gg>
      
      $guides
      <Guides[0] ggproto object>
      
      <empty>
      
      $mapping
      Aesthetic mapping: 
      * `x` -> `.data$time`
      * `y` -> `.data$model`
      
      $theme
      List of 136
       $ line                            :List of 6
        ..$ colour       : chr "black"
        ..$ linewidth    : num 0.5
        ..$ linetype     : num 1
        ..$ lineend      : chr "butt"
        ..$ arrow        : logi FALSE
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_line" "element"
       $ rect                            :List of 5
        ..$ fill         : chr "white"
        ..$ colour       : chr "black"
        ..$ linewidth    : num 0.5
        ..$ linetype     : num 1
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_rect" "element"
       $ text                            :List of 11
        ..$ family       : chr ""
        ..$ face         : chr "plain"
        ..$ colour       : chr "black"
        ..$ size         : num 11
        ..$ hjust        : num 0.5
        ..$ vjust        : num 0.5
        ..$ angle        : num 0
        ..$ lineheight   : num 0.9
        ..$ margin       : 'margin' num [1:4] 0points 0points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : logi FALSE
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ title                           : NULL
       $ aspect.ratio                    : NULL
       $ axis.title                      : NULL
       $ axis.title.x                    :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 1
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 2.75points 0points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.title.x.top                :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 0
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 0points 2.75points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.title.x.bottom             : NULL
       $ axis.title.y                    :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 1
        ..$ angle        : num 90
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 2.75points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.title.y.left               : NULL
       $ axis.title.y.right              :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 1
        ..$ angle        : num -90
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 0points 0points 2.75points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.text                       :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : chr "grey30"
        ..$ size         : 'rel' num 0.8
        ..$ hjust        : NULL
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : NULL
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.text.x                     :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 1
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 2.2points 0points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.text.x.top                 :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 0
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 0points 2.2points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.text.x.bottom              : NULL
       $ axis.text.y                     :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : num 1
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 2.2points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.text.y.left                : NULL
       $ axis.text.y.right               :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : num 0
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 0points 0points 2.2points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.text.theta                 : NULL
       $ axis.text.r                     :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : num 0.5
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 2.2points 0points 2.2points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.ticks                      :List of 6
        ..$ colour       : chr "grey20"
        ..$ linewidth    : NULL
        ..$ linetype     : NULL
        ..$ lineend      : NULL
        ..$ arrow        : logi FALSE
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_line" "element"
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
       $ axis.ticks.length               : 'simpleUnit' num 2.75points
        ..- attr(*, "unit")= int 8
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
       $ axis.line                       : list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ axis.line.x                     : NULL
       $ axis.line.x.top                 : NULL
       $ axis.line.x.bottom              : NULL
       $ axis.line.y                     : NULL
       $ axis.line.y.left                : NULL
       $ axis.line.y.right               : NULL
       $ axis.line.theta                 : NULL
       $ axis.line.r                     : NULL
       $ legend.background               :List of 5
        ..$ fill         : NULL
        ..$ colour       : logi NA
        ..$ linewidth    : NULL
        ..$ linetype     : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_rect" "element"
       $ legend.margin                   : 'margin' num [1:4] 5.5points 5.5points 5.5points 5.5points
        ..- attr(*, "unit")= int 8
       $ legend.spacing                  : 'simpleUnit' num 11points
        ..- attr(*, "unit")= int 8
       $ legend.spacing.x                : NULL
       $ legend.spacing.y                : NULL
       $ legend.key                      : NULL
       $ legend.key.size                 : 'simpleUnit' num 1.2lines
        ..- attr(*, "unit")= int 3
       $ legend.key.height               : NULL
       $ legend.key.width                : NULL
       $ legend.key.spacing              : 'simpleUnit' num 5.5points
        ..- attr(*, "unit")= int 8
       $ legend.key.spacing.x            : NULL
       $ legend.key.spacing.y            : NULL
       $ legend.frame                    : NULL
       $ legend.ticks                    : NULL
       $ legend.ticks.length             : 'rel' num 0.2
       $ legend.axis.line                : NULL
       $ legend.text                     :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : 'rel' num 0.8
        ..$ hjust        : NULL
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : NULL
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ legend.text.position            : NULL
       $ legend.title                    :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : num 0
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : NULL
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ legend.title.position           : NULL
       $ legend.position                 : chr "bottom"
       $ legend.position.inside          : NULL
       $ legend.direction                : NULL
       $ legend.byrow                    : NULL
       $ legend.justification            : chr "center"
       $ legend.justification.top        : NULL
       $ legend.justification.bottom     : NULL
       $ legend.justification.left       : NULL
       $ legend.justification.right      : NULL
       $ legend.justification.inside     : NULL
       $ legend.location                 : NULL
       $ legend.box                      : NULL
       $ legend.box.just                 : NULL
       $ legend.box.margin               : 'margin' num [1:4] 0cm 0cm 0cm 0cm
        ..- attr(*, "unit")= int 1
       $ legend.box.background           : list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ legend.box.spacing              : 'simpleUnit' num 11points
        ..- attr(*, "unit")= int 8
        [list output truncated]
       - attr(*, "class")= chr [1:2] "theme" "gg"
       - attr(*, "complete")= logi TRUE
       - attr(*, "validate")= logi TRUE
      
      $coordinates
      <ggproto object: Class CoordCartesian, Coord, gg>
          aspect: function
          backtransform_range: function
          clip: on
          default: TRUE
          distance: function
          expand: TRUE
          is_free: function
          is_linear: function
          labels: function
          limits: list
          modify_scales: function
          range: function
          render_axis_h: function
          render_axis_v: function
          render_bg: function
          render_fg: function
          setup_data: function
          setup_layout: function
          setup_panel_guides: function
          setup_panel_params: function
          setup_params: function
          train_panel_guides: function
          transform: function
          super:  <ggproto object: Class CoordCartesian, Coord, gg>
      
      $facet
      <ggproto object: Class FacetGrid, Facet, gg>
          compute_layout: function
          draw_back: function
          draw_front: function
          draw_labels: function
          draw_panels: function
          finish_data: function
          init_scales: function
          map_data: function
          params: list
          setup_data: function
          setup_params: function
          shrink: TRUE
          train_scales: function
          vars: function
          super:  <ggproto object: Class FacetGrid, Facet, gg>
      
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
      $labels$x
      NULL
      
      $labels$y
      NULL
      
      $labels$title
      NULL
      
      $labels$fill
      [1] "effect"
      
      

---

    Code
      p
    Output
      $data
                     id time coef      effect     model
      1         Austria 1995   NA -0.07548398 0.05; CO2
      2         Austria 1996   NA          NA 0.05; CO2
      3         Austria 1997   NA -0.07374097 0.05; CO2
      4         Austria 1998   NA          NA 0.05; CO2
      5         Austria 1999   NA          NA 0.05; CO2
      6         Austria 2000   NA          NA 0.05; CO2
      7         Austria 2001   NA          NA 0.05; CO2
      8         Austria 2002   NA          NA 0.05; CO2
      9         Austria 2003   NA          NA 0.05; CO2
      10        Austria 2004   NA          NA 0.05; CO2
      11        Austria 2005   NA          NA 0.05; CO2
      12        Austria 2006   NA          NA 0.05; CO2
      13        Austria 2007   NA          NA 0.05; CO2
      14        Austria 2008   NA          NA 0.05; CO2
      15        Austria 2009   NA          NA 0.05; CO2
      16        Austria 2010   NA          NA 0.05; CO2
      17        Austria 2011   NA          NA 0.05; CO2
      18        Austria 2012   NA          NA 0.05; CO2
      19        Austria 2013   NA          NA 0.05; CO2
      20        Austria 2014   NA          NA 0.05; CO2
      21        Austria 2015   NA          NA 0.05; CO2
      22        Austria 2016   NA          NA 0.05; CO2
      23        Austria 2017   NA          NA 0.05; CO2
      24        Austria 2018   NA          NA 0.05; CO2
      25        Belgium 1995   NA          NA 0.05; CO2
      26        Belgium 1996   NA          NA 0.05; CO2
      27        Belgium 1997   NA          NA 0.05; CO2
      28        Belgium 1998   NA          NA 0.05; CO2
      29        Belgium 1999   NA          NA 0.05; CO2
      30        Belgium 2000   NA          NA 0.05; CO2
      31        Belgium 2001   NA          NA 0.05; CO2
      32        Belgium 2002   NA          NA 0.05; CO2
      33        Belgium 2003   NA          NA 0.05; CO2
      34        Belgium 2004   NA          NA 0.05; CO2
      35        Belgium 2005   NA          NA 0.05; CO2
      36        Belgium 2006   NA          NA 0.05; CO2
      37        Belgium 2007   NA          NA 0.05; CO2
      38        Belgium 2008   NA          NA 0.05; CO2
      39        Belgium 2009   NA          NA 0.05; CO2
      40        Belgium 2010   NA          NA 0.05; CO2
      41        Belgium 2011   NA          NA 0.05; CO2
      42        Belgium 2012   NA          NA 0.05; CO2
      43        Belgium 2013   NA          NA 0.05; CO2
      44        Belgium 2014   NA          NA 0.05; CO2
      45        Belgium 2015   NA          NA 0.05; CO2
      46        Belgium 2016   NA          NA 0.05; CO2
      47        Belgium 2017   NA          NA 0.05; CO2
      48        Belgium 2018   NA          NA 0.05; CO2
      49        Denmark 1995   NA          NA 0.05; CO2
      50        Denmark 1996   NA          NA 0.05; CO2
      51        Denmark 1997   NA          NA 0.05; CO2
      52        Denmark 1998   NA          NA 0.05; CO2
      53        Denmark 1999   NA          NA 0.05; CO2
      54        Denmark 2000   NA          NA 0.05; CO2
      55        Denmark 2001   NA          NA 0.05; CO2
      56        Denmark 2002   NA          NA 0.05; CO2
      57        Denmark 2003   NA          NA 0.05; CO2
      58        Denmark 2004   NA          NA 0.05; CO2
      59        Denmark 2005   NA          NA 0.05; CO2
      60        Denmark 2006   NA          NA 0.05; CO2
      61        Denmark 2007   NA          NA 0.05; CO2
      62        Denmark 2008   NA          NA 0.05; CO2
      63        Denmark 2009   NA          NA 0.05; CO2
      64        Denmark 2010   NA          NA 0.05; CO2
      65        Denmark 2011   NA          NA 0.05; CO2
      66        Denmark 2012   NA          NA 0.05; CO2
      67        Denmark 2013   NA          NA 0.05; CO2
      68        Denmark 2014   NA          NA 0.05; CO2
      69        Denmark 2015   NA          NA 0.05; CO2
      70        Denmark 2016   NA          NA 0.05; CO2
      71        Denmark 2017   NA          NA 0.05; CO2
      72        Denmark 2018   NA          NA 0.05; CO2
      73        Finland 1995   NA  0.08888415 0.05; CO2
      74        Finland 1996   NA          NA 0.05; CO2
      75        Finland 1997   NA          NA 0.05; CO2
      76        Finland 1998   NA          NA 0.05; CO2
      77        Finland 1999   NA          NA 0.05; CO2
      78        Finland 2000   NA          NA 0.05; CO2
      79        Finland 2001   NA          NA 0.05; CO2
      80        Finland 2002   NA          NA 0.05; CO2
      81        Finland 2003   NA          NA 0.05; CO2
      82        Finland 2004   NA          NA 0.05; CO2
      83        Finland 2005   NA          NA 0.05; CO2
      84        Finland 2006   NA          NA 0.05; CO2
      85        Finland 2007   NA          NA 0.05; CO2
      86        Finland 2008   NA          NA 0.05; CO2
      87        Finland 2009   NA          NA 0.05; CO2
      88        Finland 2010   NA          NA 0.05; CO2
      89        Finland 2011   NA          NA 0.05; CO2
      90        Finland 2012   NA          NA 0.05; CO2
      91        Finland 2013   NA          NA 0.05; CO2
      92        Finland 2014   NA          NA 0.05; CO2
      93        Finland 2015   NA          NA 0.05; CO2
      94        Finland 2016   NA          NA 0.05; CO2
      95        Finland 2017   NA          NA 0.05; CO2
      96        Finland 2018   NA          NA 0.05; CO2
      97         France 1995   NA          NA 0.05; CO2
      98         France 1996   NA          NA 0.05; CO2
      99         France 1997   NA          NA 0.05; CO2
      100        France 1998   NA          NA 0.05; CO2
      101        France 1999   NA          NA 0.05; CO2
      102        France 2000   NA          NA 0.05; CO2
      103        France 2001   NA          NA 0.05; CO2
      104        France 2002   NA          NA 0.05; CO2
      105        France 2003   NA          NA 0.05; CO2
      106        France 2004   NA          NA 0.05; CO2
      107        France 2005   NA          NA 0.05; CO2
      108        France 2006   NA          NA 0.05; CO2
      109        France 2007   NA          NA 0.05; CO2
      110        France 2008   NA          NA 0.05; CO2
      111        France 2009   NA          NA 0.05; CO2
      112        France 2010   NA          NA 0.05; CO2
      113        France 2011   NA          NA 0.05; CO2
      114        France 2012   NA          NA 0.05; CO2
      115        France 2013   NA          NA 0.05; CO2
      116        France 2014   NA          NA 0.05; CO2
      117        France 2015   NA          NA 0.05; CO2
      118        France 2016   NA          NA 0.05; CO2
      119        France 2017   NA          NA 0.05; CO2
      120        France 2018   NA          NA 0.05; CO2
      121       Germany 1995   NA          NA 0.05; CO2
      122       Germany 1996   NA          NA 0.05; CO2
      123       Germany 1997   NA          NA 0.05; CO2
      124       Germany 1998   NA          NA 0.05; CO2
      125       Germany 1999   NA          NA 0.05; CO2
      126       Germany 2000   NA          NA 0.05; CO2
      127       Germany 2001   NA          NA 0.05; CO2
      128       Germany 2002   NA          NA 0.05; CO2
      129       Germany 2003   NA          NA 0.05; CO2
      130       Germany 2004   NA          NA 0.05; CO2
      131       Germany 2005   NA          NA 0.05; CO2
      132       Germany 2006   NA          NA 0.05; CO2
      133       Germany 2007   NA -0.07316183 0.05; CO2
      134       Germany 2008   NA          NA 0.05; CO2
      135       Germany 2009   NA          NA 0.05; CO2
      136       Germany 2010   NA          NA 0.05; CO2
      137       Germany 2011   NA          NA 0.05; CO2
      138       Germany 2012   NA          NA 0.05; CO2
      139       Germany 2013   NA          NA 0.05; CO2
      140       Germany 2014   NA          NA 0.05; CO2
      141       Germany 2015   NA          NA 0.05; CO2
      142       Germany 2016   NA          NA 0.05; CO2
      143       Germany 2017   NA          NA 0.05; CO2
      144       Germany 2018   NA          NA 0.05; CO2
      145        Greece 1995   NA          NA 0.05; CO2
      146        Greece 1996   NA          NA 0.05; CO2
      147        Greece 1997   NA          NA 0.05; CO2
      148        Greece 1998   NA          NA 0.05; CO2
      149        Greece 1999   NA          NA 0.05; CO2
      150        Greece 2000   NA          NA 0.05; CO2
      151        Greece 2001   NA          NA 0.05; CO2
      152        Greece 2002   NA          NA 0.05; CO2
      153        Greece 2003   NA          NA 0.05; CO2
      154        Greece 2004   NA          NA 0.05; CO2
      155        Greece 2005   NA          NA 0.05; CO2
      156        Greece 2006   NA          NA 0.05; CO2
      157        Greece 2007   NA          NA 0.05; CO2
      158        Greece 2008   NA          NA 0.05; CO2
      159        Greece 2009   NA          NA 0.05; CO2
      160        Greece 2010   NA          NA 0.05; CO2
      161        Greece 2011   NA          NA 0.05; CO2
      162        Greece 2012   NA          NA 0.05; CO2
      163        Greece 2013   NA          NA 0.05; CO2
      164        Greece 2014   NA          NA 0.05; CO2
      165        Greece 2015   NA          NA 0.05; CO2
      166        Greece 2016   NA          NA 0.05; CO2
      167        Greece 2017   NA          NA 0.05; CO2
      168        Greece 2018   NA          NA 0.05; CO2
      169       Ireland 1995   NA          NA 0.05; CO2
      170       Ireland 1996   NA          NA 0.05; CO2
      171       Ireland 1997   NA          NA 0.05; CO2
      172       Ireland 1998   NA          NA 0.05; CO2
      173       Ireland 1999   NA          NA 0.05; CO2
      174       Ireland 2000   NA          NA 0.05; CO2
      175       Ireland 2001   NA          NA 0.05; CO2
      176       Ireland 2002   NA          NA 0.05; CO2
      177       Ireland 2003   NA          NA 0.05; CO2
      178       Ireland 2004   NA          NA 0.05; CO2
      179       Ireland 2005   NA          NA 0.05; CO2
      180       Ireland 2006   NA          NA 0.05; CO2
      181       Ireland 2007   NA          NA 0.05; CO2
      182       Ireland 2008   NA          NA 0.05; CO2
      183       Ireland 2009   NA          NA 0.05; CO2
      184       Ireland 2010   NA          NA 0.05; CO2
      185       Ireland 2011   NA          NA 0.05; CO2
      186       Ireland 2012   NA          NA 0.05; CO2
      187       Ireland 2013   NA          NA 0.05; CO2
      188       Ireland 2014   NA          NA 0.05; CO2
      189       Ireland 2015   NA          NA 0.05; CO2
      190       Ireland 2016   NA          NA 0.05; CO2
      191       Ireland 2017   NA          NA 0.05; CO2
      192       Ireland 2018   NA          NA 0.05; CO2
      193         Italy 1995   NA          NA 0.05; CO2
      194         Italy 1996   NA          NA 0.05; CO2
      195         Italy 1997   NA          NA 0.05; CO2
      196         Italy 1998   NA          NA 0.05; CO2
      197         Italy 1999   NA          NA 0.05; CO2
      198         Italy 2000   NA          NA 0.05; CO2
      199         Italy 2001   NA          NA 0.05; CO2
      200         Italy 2002   NA          NA 0.05; CO2
      201         Italy 2003   NA          NA 0.05; CO2
      202         Italy 2004   NA          NA 0.05; CO2
      203         Italy 2005   NA          NA 0.05; CO2
      204         Italy 2006   NA          NA 0.05; CO2
      205         Italy 2007   NA          NA 0.05; CO2
      206         Italy 2008   NA          NA 0.05; CO2
      207         Italy 2009   NA          NA 0.05; CO2
      208         Italy 2010   NA          NA 0.05; CO2
      209         Italy 2011   NA          NA 0.05; CO2
      210         Italy 2012   NA          NA 0.05; CO2
      211         Italy 2013   NA          NA 0.05; CO2
      212         Italy 2014   NA          NA 0.05; CO2
      213         Italy 2015   NA          NA 0.05; CO2
      214         Italy 2016   NA          NA 0.05; CO2
      215         Italy 2017   NA          NA 0.05; CO2
      216         Italy 2018   NA          NA 0.05; CO2
      217    Luxembourg 1995   NA          NA 0.05; CO2
      218    Luxembourg 1996   NA          NA 0.05; CO2
      219    Luxembourg 1997   NA          NA 0.05; CO2
      220    Luxembourg 1998   NA          NA 0.05; CO2
      221    Luxembourg 1999   NA          NA 0.05; CO2
      222    Luxembourg 2000   NA          NA 0.05; CO2
      223    Luxembourg 2001   NA          NA 0.05; CO2
      224    Luxembourg 2002   NA          NA 0.05; CO2
      225    Luxembourg 2003   NA          NA 0.05; CO2
      226    Luxembourg 2004   NA          NA 0.05; CO2
      227    Luxembourg 2005   NA          NA 0.05; CO2
      228    Luxembourg 2006   NA          NA 0.05; CO2
      229    Luxembourg 2007   NA          NA 0.05; CO2
      230    Luxembourg 2008   NA          NA 0.05; CO2
      231    Luxembourg 2009   NA          NA 0.05; CO2
      232    Luxembourg 2010   NA          NA 0.05; CO2
      233    Luxembourg 2011   NA          NA 0.05; CO2
      234    Luxembourg 2012   NA          NA 0.05; CO2
      235    Luxembourg 2013   NA          NA 0.05; CO2
      236    Luxembourg 2014   NA          NA 0.05; CO2
      237    Luxembourg 2015   NA -0.19991527 0.05; CO2
      238    Luxembourg 2016   NA -0.30421219 0.05; CO2
      239    Luxembourg 2017   NA -0.27162454 0.05; CO2
      240    Luxembourg 2018   NA -0.24126820 0.05; CO2
      241   Netherlands 1995   NA          NA 0.05; CO2
      242   Netherlands 1996   NA          NA 0.05; CO2
      243   Netherlands 1997   NA          NA 0.05; CO2
      244   Netherlands 1998   NA          NA 0.05; CO2
      245   Netherlands 1999   NA          NA 0.05; CO2
      246   Netherlands 2000   NA          NA 0.05; CO2
      247   Netherlands 2001   NA          NA 0.05; CO2
      248   Netherlands 2002   NA          NA 0.05; CO2
      249   Netherlands 2003   NA          NA 0.05; CO2
      250   Netherlands 2004   NA          NA 0.05; CO2
      251   Netherlands 2005   NA          NA 0.05; CO2
      252   Netherlands 2006   NA          NA 0.05; CO2
      253   Netherlands 2007   NA          NA 0.05; CO2
      254   Netherlands 2008   NA          NA 0.05; CO2
      255   Netherlands 2009   NA          NA 0.05; CO2
      256   Netherlands 2010   NA          NA 0.05; CO2
      257   Netherlands 2011   NA          NA 0.05; CO2
      258   Netherlands 2012   NA          NA 0.05; CO2
      259   Netherlands 2013   NA          NA 0.05; CO2
      260   Netherlands 2014   NA          NA 0.05; CO2
      261   Netherlands 2015   NA          NA 0.05; CO2
      262   Netherlands 2016   NA          NA 0.05; CO2
      263   Netherlands 2017   NA          NA 0.05; CO2
      264   Netherlands 2018   NA          NA 0.05; CO2
      265      Portugal 1995   NA          NA 0.05; CO2
      266      Portugal 1996   NA          NA 0.05; CO2
      267      Portugal 1997   NA          NA 0.05; CO2
      268      Portugal 1998   NA          NA 0.05; CO2
      269      Portugal 1999   NA          NA 0.05; CO2
      270      Portugal 2000   NA          NA 0.05; CO2
      271      Portugal 2001   NA          NA 0.05; CO2
      272      Portugal 2002   NA          NA 0.05; CO2
      273      Portugal 2003   NA          NA 0.05; CO2
      274      Portugal 2004   NA          NA 0.05; CO2
      275      Portugal 2005   NA          NA 0.05; CO2
      276      Portugal 2006   NA          NA 0.05; CO2
      277      Portugal 2007   NA          NA 0.05; CO2
      278      Portugal 2008   NA          NA 0.05; CO2
      279      Portugal 2009   NA          NA 0.05; CO2
      280      Portugal 2010   NA          NA 0.05; CO2
      281      Portugal 2011   NA          NA 0.05; CO2
      282      Portugal 2012   NA          NA 0.05; CO2
      283      Portugal 2013   NA          NA 0.05; CO2
      284      Portugal 2014   NA          NA 0.05; CO2
      285      Portugal 2015   NA          NA 0.05; CO2
      286      Portugal 2016   NA          NA 0.05; CO2
      287      Portugal 2017   NA          NA 0.05; CO2
      288      Portugal 2018   NA          NA 0.05; CO2
      289         Spain 1995   NA -0.13634923 0.05; CO2
      290         Spain 1996   NA -0.10507874 0.05; CO2
      291         Spain 1997   NA -0.10243723 0.05; CO2
      292         Spain 1998   NA          NA 0.05; CO2
      293         Spain 1999   NA          NA 0.05; CO2
      294         Spain 2000   NA          NA 0.05; CO2
      295         Spain 2001   NA          NA 0.05; CO2
      296         Spain 2002   NA          NA 0.05; CO2
      297         Spain 2003   NA          NA 0.05; CO2
      298         Spain 2004   NA          NA 0.05; CO2
      299         Spain 2005   NA          NA 0.05; CO2
      300         Spain 2006   NA          NA 0.05; CO2
      301         Spain 2007   NA          NA 0.05; CO2
      302         Spain 2008   NA          NA 0.05; CO2
      303         Spain 2009   NA          NA 0.05; CO2
      304         Spain 2010   NA          NA 0.05; CO2
      305         Spain 2011   NA          NA 0.05; CO2
      306         Spain 2012   NA          NA 0.05; CO2
      307         Spain 2013   NA          NA 0.05; CO2
      308         Spain 2014   NA          NA 0.05; CO2
      309         Spain 2015   NA          NA 0.05; CO2
      310         Spain 2016   NA          NA 0.05; CO2
      311         Spain 2017   NA          NA 0.05; CO2
      312         Spain 2018   NA          NA 0.05; CO2
      313        Sweden 1995   NA          NA 0.05; CO2
      314        Sweden 1996   NA          NA 0.05; CO2
      315        Sweden 1997   NA          NA 0.05; CO2
      316        Sweden 1998   NA          NA 0.05; CO2
      317        Sweden 1999   NA          NA 0.05; CO2
      318        Sweden 2000   NA          NA 0.05; CO2
      319        Sweden 2001   NA          NA 0.05; CO2
      320        Sweden 2002   NA          NA 0.05; CO2
      321        Sweden 2003   NA          NA 0.05; CO2
      322        Sweden 2004   NA          NA 0.05; CO2
      323        Sweden 2005   NA          NA 0.05; CO2
      324        Sweden 2006   NA          NA 0.05; CO2
      325        Sweden 2007   NA          NA 0.05; CO2
      326        Sweden 2008   NA          NA 0.05; CO2
      327        Sweden 2009   NA          NA 0.05; CO2
      328        Sweden 2010   NA          NA 0.05; CO2
      329        Sweden 2011   NA          NA 0.05; CO2
      330        Sweden 2012   NA          NA 0.05; CO2
      331        Sweden 2013   NA          NA 0.05; CO2
      332        Sweden 2014   NA          NA 0.05; CO2
      333        Sweden 2015   NA          NA 0.05; CO2
      334        Sweden 2016   NA          NA 0.05; CO2
      335        Sweden 2017   NA          NA 0.05; CO2
      336        Sweden 2018   NA          NA 0.05; CO2
      337 UnitedKingdom 1995   NA          NA 0.05; CO2
      338 UnitedKingdom 1996   NA          NA 0.05; CO2
      339 UnitedKingdom 1997   NA          NA 0.05; CO2
      340 UnitedKingdom 1998   NA          NA 0.05; CO2
      341 UnitedKingdom 1999   NA          NA 0.05; CO2
      342 UnitedKingdom 2000   NA          NA 0.05; CO2
      343 UnitedKingdom 2001   NA          NA 0.05; CO2
      344 UnitedKingdom 2002   NA          NA 0.05; CO2
      345 UnitedKingdom 2003   NA          NA 0.05; CO2
      346 UnitedKingdom 2004   NA          NA 0.05; CO2
      347 UnitedKingdom 2005   NA          NA 0.05; CO2
      348 UnitedKingdom 2006   NA          NA 0.05; CO2
      349 UnitedKingdom 2007   NA          NA 0.05; CO2
      350 UnitedKingdom 2008   NA          NA 0.05; CO2
      351 UnitedKingdom 2009   NA          NA 0.05; CO2
      352 UnitedKingdom 2010   NA          NA 0.05; CO2
      353 UnitedKingdom 2011   NA          NA 0.05; CO2
      354 UnitedKingdom 2012   NA          NA 0.05; CO2
      355 UnitedKingdom 2013   NA          NA 0.05; CO2
      356 UnitedKingdom 2014   NA          NA 0.05; CO2
      357 UnitedKingdom 2015   NA          NA 0.05; CO2
      358 UnitedKingdom 2016   NA          NA 0.05; CO2
      359 UnitedKingdom 2017   NA          NA 0.05; CO2
      360 UnitedKingdom 2018   NA          NA 0.05; CO2
      361       Austria 1995   NA          NA 0.01; CO2
      362       Austria 1996   NA          NA 0.01; CO2
      363       Austria 1997   NA          NA 0.01; CO2
      364       Austria 1998   NA          NA 0.01; CO2
      365       Austria 1999   NA          NA 0.01; CO2
      366       Austria 2000   NA          NA 0.01; CO2
      367       Austria 2001   NA          NA 0.01; CO2
      368       Austria 2002   NA          NA 0.01; CO2
      369       Austria 2003   NA          NA 0.01; CO2
      370       Austria 2004   NA          NA 0.01; CO2
      371       Austria 2005   NA          NA 0.01; CO2
      372       Austria 2006   NA          NA 0.01; CO2
      373       Austria 2007   NA          NA 0.01; CO2
      374       Austria 2008   NA          NA 0.01; CO2
      375       Austria 2009   NA          NA 0.01; CO2
      376       Austria 2010   NA          NA 0.01; CO2
      377       Austria 2011   NA          NA 0.01; CO2
      378       Austria 2012   NA          NA 0.01; CO2
      379       Austria 2013   NA          NA 0.01; CO2
      380       Austria 2014   NA          NA 0.01; CO2
      381       Austria 2015   NA          NA 0.01; CO2
      382       Austria 2016   NA          NA 0.01; CO2
      383       Austria 2017   NA          NA 0.01; CO2
      384       Austria 2018   NA          NA 0.01; CO2
      385       Belgium 1995   NA          NA 0.01; CO2
      386       Belgium 1996   NA          NA 0.01; CO2
      387       Belgium 1997   NA          NA 0.01; CO2
      388       Belgium 1998   NA          NA 0.01; CO2
      389       Belgium 1999   NA          NA 0.01; CO2
      390       Belgium 2000   NA          NA 0.01; CO2
      391       Belgium 2001   NA          NA 0.01; CO2
      392       Belgium 2002   NA          NA 0.01; CO2
      393       Belgium 2003   NA          NA 0.01; CO2
      394       Belgium 2004   NA          NA 0.01; CO2
      395       Belgium 2005   NA          NA 0.01; CO2
      396       Belgium 2006   NA          NA 0.01; CO2
      397       Belgium 2007   NA          NA 0.01; CO2
      398       Belgium 2008   NA          NA 0.01; CO2
      399       Belgium 2009   NA          NA 0.01; CO2
      400       Belgium 2010   NA          NA 0.01; CO2
      401       Belgium 2011   NA          NA 0.01; CO2
      402       Belgium 2012   NA          NA 0.01; CO2
      403       Belgium 2013   NA          NA 0.01; CO2
      404       Belgium 2014   NA          NA 0.01; CO2
      405       Belgium 2015   NA          NA 0.01; CO2
      406       Belgium 2016   NA          NA 0.01; CO2
      407       Belgium 2017   NA          NA 0.01; CO2
      408       Belgium 2018   NA          NA 0.01; CO2
      409       Denmark 1995   NA          NA 0.01; CO2
      410       Denmark 1996   NA          NA 0.01; CO2
      411       Denmark 1997   NA          NA 0.01; CO2
      412       Denmark 1998   NA          NA 0.01; CO2
      413       Denmark 1999   NA          NA 0.01; CO2
      414       Denmark 2000   NA          NA 0.01; CO2
      415       Denmark 2001   NA          NA 0.01; CO2
      416       Denmark 2002   NA          NA 0.01; CO2
      417       Denmark 2003   NA          NA 0.01; CO2
      418       Denmark 2004   NA          NA 0.01; CO2
      419       Denmark 2005   NA          NA 0.01; CO2
      420       Denmark 2006   NA          NA 0.01; CO2
      421       Denmark 2007   NA          NA 0.01; CO2
      422       Denmark 2008   NA          NA 0.01; CO2
      423       Denmark 2009   NA          NA 0.01; CO2
      424       Denmark 2010   NA          NA 0.01; CO2
      425       Denmark 2011   NA          NA 0.01; CO2
      426       Denmark 2012   NA          NA 0.01; CO2
      427       Denmark 2013   NA          NA 0.01; CO2
      428       Denmark 2014   NA          NA 0.01; CO2
      429       Denmark 2015   NA          NA 0.01; CO2
      430       Denmark 2016   NA          NA 0.01; CO2
      431       Denmark 2017   NA          NA 0.01; CO2
      432       Denmark 2018   NA          NA 0.01; CO2
      433       Finland 1995   NA          NA 0.01; CO2
      434       Finland 1996   NA          NA 0.01; CO2
      435       Finland 1997   NA          NA 0.01; CO2
      436       Finland 1998   NA          NA 0.01; CO2
      437       Finland 1999   NA          NA 0.01; CO2
      438       Finland 2000   NA          NA 0.01; CO2
      439       Finland 2001   NA          NA 0.01; CO2
      440       Finland 2002   NA          NA 0.01; CO2
      441       Finland 2003   NA          NA 0.01; CO2
      442       Finland 2004   NA          NA 0.01; CO2
      443       Finland 2005   NA          NA 0.01; CO2
      444       Finland 2006   NA          NA 0.01; CO2
      445       Finland 2007   NA          NA 0.01; CO2
      446       Finland 2008   NA          NA 0.01; CO2
      447       Finland 2009   NA          NA 0.01; CO2
      448       Finland 2010   NA          NA 0.01; CO2
      449       Finland 2011   NA          NA 0.01; CO2
      450       Finland 2012   NA          NA 0.01; CO2
      451       Finland 2013   NA          NA 0.01; CO2
      452       Finland 2014   NA          NA 0.01; CO2
      453       Finland 2015   NA          NA 0.01; CO2
      454       Finland 2016   NA          NA 0.01; CO2
      455       Finland 2017   NA          NA 0.01; CO2
      456       Finland 2018   NA          NA 0.01; CO2
      457        France 1995   NA          NA 0.01; CO2
      458        France 1996   NA          NA 0.01; CO2
      459        France 1997   NA          NA 0.01; CO2
      460        France 1998   NA          NA 0.01; CO2
      461        France 1999   NA          NA 0.01; CO2
      462        France 2000   NA          NA 0.01; CO2
      463        France 2001   NA          NA 0.01; CO2
      464        France 2002   NA          NA 0.01; CO2
      465        France 2003   NA          NA 0.01; CO2
      466        France 2004   NA          NA 0.01; CO2
      467        France 2005   NA          NA 0.01; CO2
      468        France 2006   NA          NA 0.01; CO2
      469        France 2007   NA          NA 0.01; CO2
      470        France 2008   NA          NA 0.01; CO2
      471        France 2009   NA          NA 0.01; CO2
      472        France 2010   NA          NA 0.01; CO2
      473        France 2011   NA          NA 0.01; CO2
      474        France 2012   NA          NA 0.01; CO2
      475        France 2013   NA          NA 0.01; CO2
      476        France 2014   NA          NA 0.01; CO2
      477        France 2015   NA          NA 0.01; CO2
      478        France 2016   NA          NA 0.01; CO2
      479        France 2017   NA          NA 0.01; CO2
      480        France 2018   NA          NA 0.01; CO2
      481       Germany 1995   NA          NA 0.01; CO2
      482       Germany 1996   NA          NA 0.01; CO2
      483       Germany 1997   NA          NA 0.01; CO2
      484       Germany 1998   NA          NA 0.01; CO2
      485       Germany 1999   NA          NA 0.01; CO2
      486       Germany 2000   NA          NA 0.01; CO2
      487       Germany 2001   NA          NA 0.01; CO2
      488       Germany 2002   NA          NA 0.01; CO2
      489       Germany 2003   NA          NA 0.01; CO2
      490       Germany 2004   NA          NA 0.01; CO2
      491       Germany 2005   NA          NA 0.01; CO2
      492       Germany 2006   NA          NA 0.01; CO2
      493       Germany 2007   NA          NA 0.01; CO2
      494       Germany 2008   NA          NA 0.01; CO2
      495       Germany 2009   NA          NA 0.01; CO2
      496       Germany 2010   NA          NA 0.01; CO2
      497       Germany 2011   NA          NA 0.01; CO2
      498       Germany 2012   NA          NA 0.01; CO2
      499       Germany 2013   NA          NA 0.01; CO2
      500       Germany 2014   NA          NA 0.01; CO2
      501       Germany 2015   NA          NA 0.01; CO2
      502       Germany 2016   NA          NA 0.01; CO2
      503       Germany 2017   NA          NA 0.01; CO2
      504       Germany 2018   NA          NA 0.01; CO2
      505        Greece 1995   NA          NA 0.01; CO2
      506        Greece 1996   NA          NA 0.01; CO2
      507        Greece 1997   NA          NA 0.01; CO2
      508        Greece 1998   NA          NA 0.01; CO2
      509        Greece 1999   NA          NA 0.01; CO2
      510        Greece 2000   NA          NA 0.01; CO2
      511        Greece 2001   NA          NA 0.01; CO2
      512        Greece 2002   NA          NA 0.01; CO2
      513        Greece 2003   NA          NA 0.01; CO2
      514        Greece 2004   NA          NA 0.01; CO2
      515        Greece 2005   NA          NA 0.01; CO2
      516        Greece 2006   NA          NA 0.01; CO2
      517        Greece 2007   NA          NA 0.01; CO2
      518        Greece 2008   NA          NA 0.01; CO2
      519        Greece 2009   NA          NA 0.01; CO2
      520        Greece 2010   NA          NA 0.01; CO2
      521        Greece 2011   NA          NA 0.01; CO2
      522        Greece 2012   NA          NA 0.01; CO2
      523        Greece 2013   NA          NA 0.01; CO2
      524        Greece 2014   NA          NA 0.01; CO2
      525        Greece 2015   NA          NA 0.01; CO2
      526        Greece 2016   NA          NA 0.01; CO2
      527        Greece 2017   NA          NA 0.01; CO2
      528        Greece 2018   NA          NA 0.01; CO2
      529       Ireland 1995   NA          NA 0.01; CO2
      530       Ireland 1996   NA          NA 0.01; CO2
      531       Ireland 1997   NA          NA 0.01; CO2
      532       Ireland 1998   NA          NA 0.01; CO2
      533       Ireland 1999   NA          NA 0.01; CO2
      534       Ireland 2000   NA          NA 0.01; CO2
      535       Ireland 2001   NA          NA 0.01; CO2
      536       Ireland 2002   NA          NA 0.01; CO2
      537       Ireland 2003   NA          NA 0.01; CO2
      538       Ireland 2004   NA          NA 0.01; CO2
      539       Ireland 2005   NA          NA 0.01; CO2
      540       Ireland 2006   NA          NA 0.01; CO2
      541       Ireland 2007   NA          NA 0.01; CO2
      542       Ireland 2008   NA          NA 0.01; CO2
      543       Ireland 2009   NA          NA 0.01; CO2
      544       Ireland 2010   NA          NA 0.01; CO2
      545       Ireland 2011   NA          NA 0.01; CO2
      546       Ireland 2012   NA          NA 0.01; CO2
      547       Ireland 2013   NA          NA 0.01; CO2
      548       Ireland 2014   NA          NA 0.01; CO2
      549       Ireland 2015   NA          NA 0.01; CO2
      550       Ireland 2016   NA          NA 0.01; CO2
      551       Ireland 2017   NA          NA 0.01; CO2
      552       Ireland 2018   NA          NA 0.01; CO2
      553         Italy 1995   NA          NA 0.01; CO2
      554         Italy 1996   NA          NA 0.01; CO2
      555         Italy 1997   NA          NA 0.01; CO2
      556         Italy 1998   NA          NA 0.01; CO2
      557         Italy 1999   NA          NA 0.01; CO2
      558         Italy 2000   NA          NA 0.01; CO2
      559         Italy 2001   NA          NA 0.01; CO2
      560         Italy 2002   NA          NA 0.01; CO2
      561         Italy 2003   NA          NA 0.01; CO2
      562         Italy 2004   NA          NA 0.01; CO2
      563         Italy 2005   NA          NA 0.01; CO2
      564         Italy 2006   NA          NA 0.01; CO2
      565         Italy 2007   NA          NA 0.01; CO2
      566         Italy 2008   NA          NA 0.01; CO2
      567         Italy 2009   NA          NA 0.01; CO2
      568         Italy 2010   NA          NA 0.01; CO2
      569         Italy 2011   NA          NA 0.01; CO2
      570         Italy 2012   NA          NA 0.01; CO2
      571         Italy 2013   NA          NA 0.01; CO2
      572         Italy 2014   NA          NA 0.01; CO2
      573         Italy 2015   NA          NA 0.01; CO2
      574         Italy 2016   NA          NA 0.01; CO2
      575         Italy 2017   NA          NA 0.01; CO2
      576         Italy 2018   NA          NA 0.01; CO2
      577    Luxembourg 1995   NA          NA 0.01; CO2
      578    Luxembourg 1996   NA          NA 0.01; CO2
      579    Luxembourg 1997   NA          NA 0.01; CO2
      580    Luxembourg 1998   NA          NA 0.01; CO2
      581    Luxembourg 1999   NA          NA 0.01; CO2
      582    Luxembourg 2000   NA          NA 0.01; CO2
      583    Luxembourg 2001   NA          NA 0.01; CO2
      584    Luxembourg 2002   NA          NA 0.01; CO2
      585    Luxembourg 2003   NA          NA 0.01; CO2
      586    Luxembourg 2004   NA  0.10958650 0.01; CO2
      587    Luxembourg 2005   NA  0.13968791 0.01; CO2
      588    Luxembourg 2006   NA          NA 0.01; CO2
      589    Luxembourg 2007   NA          NA 0.01; CO2
      590    Luxembourg 2008   NA          NA 0.01; CO2
      591    Luxembourg 2009   NA          NA 0.01; CO2
      592    Luxembourg 2010   NA          NA 0.01; CO2
      593    Luxembourg 2011   NA          NA 0.01; CO2
      594    Luxembourg 2012   NA          NA 0.01; CO2
      595    Luxembourg 2013   NA          NA 0.01; CO2
      596    Luxembourg 2014   NA          NA 0.01; CO2
      597    Luxembourg 2015   NA -0.17929908 0.01; CO2
      598    Luxembourg 2016   NA -0.27359425 0.01; CO2
      599    Luxembourg 2017   NA -0.23274999 0.01; CO2
      600    Luxembourg 2018   NA -0.19442893 0.01; CO2
      601   Netherlands 1995   NA          NA 0.01; CO2
      602   Netherlands 1996   NA          NA 0.01; CO2
      603   Netherlands 1997   NA          NA 0.01; CO2
      604   Netherlands 1998   NA          NA 0.01; CO2
      605   Netherlands 1999   NA          NA 0.01; CO2
      606   Netherlands 2000   NA          NA 0.01; CO2
      607   Netherlands 2001   NA          NA 0.01; CO2
      608   Netherlands 2002   NA          NA 0.01; CO2
      609   Netherlands 2003   NA          NA 0.01; CO2
      610   Netherlands 2004   NA          NA 0.01; CO2
      611   Netherlands 2005   NA          NA 0.01; CO2
      612   Netherlands 2006   NA          NA 0.01; CO2
      613   Netherlands 2007   NA          NA 0.01; CO2
      614   Netherlands 2008   NA          NA 0.01; CO2
      615   Netherlands 2009   NA          NA 0.01; CO2
      616   Netherlands 2010   NA          NA 0.01; CO2
      617   Netherlands 2011   NA          NA 0.01; CO2
      618   Netherlands 2012   NA          NA 0.01; CO2
      619   Netherlands 2013   NA          NA 0.01; CO2
      620   Netherlands 2014   NA          NA 0.01; CO2
      621   Netherlands 2015   NA          NA 0.01; CO2
      622   Netherlands 2016   NA          NA 0.01; CO2
      623   Netherlands 2017   NA          NA 0.01; CO2
      624   Netherlands 2018   NA          NA 0.01; CO2
      625      Portugal 1995   NA          NA 0.01; CO2
      626      Portugal 1996   NA          NA 0.01; CO2
      627      Portugal 1997   NA          NA 0.01; CO2
      628      Portugal 1998   NA          NA 0.01; CO2
      629      Portugal 1999   NA          NA 0.01; CO2
      630      Portugal 2000   NA          NA 0.01; CO2
      631      Portugal 2001   NA          NA 0.01; CO2
      632      Portugal 2002   NA          NA 0.01; CO2
      633      Portugal 2003   NA          NA 0.01; CO2
      634      Portugal 2004   NA          NA 0.01; CO2
      635      Portugal 2005   NA          NA 0.01; CO2
      636      Portugal 2006   NA          NA 0.01; CO2
      637      Portugal 2007   NA          NA 0.01; CO2
      638      Portugal 2008   NA          NA 0.01; CO2
      639      Portugal 2009   NA          NA 0.01; CO2
      640      Portugal 2010   NA          NA 0.01; CO2
      641      Portugal 2011   NA          NA 0.01; CO2
      642      Portugal 2012   NA          NA 0.01; CO2
      643      Portugal 2013   NA          NA 0.01; CO2
      644      Portugal 2014   NA          NA 0.01; CO2
      645      Portugal 2015   NA          NA 0.01; CO2
      646      Portugal 2016   NA          NA 0.01; CO2
      647      Portugal 2017   NA          NA 0.01; CO2
      648      Portugal 2018   NA          NA 0.01; CO2
      649         Spain 1995   NA          NA 0.01; CO2
      650         Spain 1996   NA          NA 0.01; CO2
      651         Spain 1997   NA          NA 0.01; CO2
      652         Spain 1998   NA          NA 0.01; CO2
      653         Spain 1999   NA          NA 0.01; CO2
      654         Spain 2000   NA          NA 0.01; CO2
      655         Spain 2001   NA          NA 0.01; CO2
      656         Spain 2002   NA          NA 0.01; CO2
      657         Spain 2003   NA          NA 0.01; CO2
      658         Spain 2004   NA          NA 0.01; CO2
      659         Spain 2005   NA          NA 0.01; CO2
      660         Spain 2006   NA          NA 0.01; CO2
      661         Spain 2007   NA          NA 0.01; CO2
      662         Spain 2008   NA          NA 0.01; CO2
      663         Spain 2009   NA          NA 0.01; CO2
      664         Spain 2010   NA          NA 0.01; CO2
      665         Spain 2011   NA          NA 0.01; CO2
      666         Spain 2012   NA          NA 0.01; CO2
      667         Spain 2013   NA          NA 0.01; CO2
      668         Spain 2014   NA          NA 0.01; CO2
      669         Spain 2015   NA          NA 0.01; CO2
      670         Spain 2016   NA          NA 0.01; CO2
      671         Spain 2017   NA          NA 0.01; CO2
      672         Spain 2018   NA          NA 0.01; CO2
      673        Sweden 1995   NA          NA 0.01; CO2
      674        Sweden 1996   NA          NA 0.01; CO2
      675        Sweden 1997   NA          NA 0.01; CO2
      676        Sweden 1998   NA          NA 0.01; CO2
      677        Sweden 1999   NA          NA 0.01; CO2
      678        Sweden 2000   NA          NA 0.01; CO2
      679        Sweden 2001   NA          NA 0.01; CO2
      680        Sweden 2002   NA          NA 0.01; CO2
      681        Sweden 2003   NA          NA 0.01; CO2
      682        Sweden 2004   NA          NA 0.01; CO2
      683        Sweden 2005   NA          NA 0.01; CO2
      684        Sweden 2006   NA          NA 0.01; CO2
      685        Sweden 2007   NA          NA 0.01; CO2
      686        Sweden 2008   NA          NA 0.01; CO2
      687        Sweden 2009   NA          NA 0.01; CO2
      688        Sweden 2010   NA          NA 0.01; CO2
      689        Sweden 2011   NA          NA 0.01; CO2
      690        Sweden 2012   NA          NA 0.01; CO2
      691        Sweden 2013   NA          NA 0.01; CO2
      692        Sweden 2014   NA          NA 0.01; CO2
      693        Sweden 2015   NA          NA 0.01; CO2
      694        Sweden 2016   NA          NA 0.01; CO2
      695        Sweden 2017   NA          NA 0.01; CO2
      696        Sweden 2018   NA          NA 0.01; CO2
      697 UnitedKingdom 1995   NA          NA 0.01; CO2
      698 UnitedKingdom 1996   NA          NA 0.01; CO2
      699 UnitedKingdom 1997   NA          NA 0.01; CO2
      700 UnitedKingdom 1998   NA          NA 0.01; CO2
      701 UnitedKingdom 1999   NA          NA 0.01; CO2
      702 UnitedKingdom 2000   NA          NA 0.01; CO2
      703 UnitedKingdom 2001   NA          NA 0.01; CO2
      704 UnitedKingdom 2002   NA          NA 0.01; CO2
      705 UnitedKingdom 2003   NA          NA 0.01; CO2
      706 UnitedKingdom 2004   NA          NA 0.01; CO2
      707 UnitedKingdom 2005   NA          NA 0.01; CO2
      708 UnitedKingdom 2006   NA          NA 0.01; CO2
      709 UnitedKingdom 2007   NA          NA 0.01; CO2
      710 UnitedKingdom 2008   NA          NA 0.01; CO2
      711 UnitedKingdom 2009   NA          NA 0.01; CO2
      712 UnitedKingdom 2010   NA          NA 0.01; CO2
      713 UnitedKingdom 2011   NA          NA 0.01; CO2
      714 UnitedKingdom 2012   NA          NA 0.01; CO2
      715 UnitedKingdom 2013   NA          NA 0.01; CO2
      716 UnitedKingdom 2014   NA          NA 0.01; CO2
      717 UnitedKingdom 2015   NA          NA 0.01; CO2
      718 UnitedKingdom 2016   NA          NA 0.01; CO2
      719 UnitedKingdom 2017   NA          NA 0.01; CO2
      720 UnitedKingdom 2018   NA          NA 0.01; CO2
      
      $layers
      $layers[[1]]
      mapping: fill = ~.data$effect 
      geom_tile: linejoin = mitre, na.rm = TRUE
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
          train_df: function
          transform_df: function
          super:  <ggproto object: Class ScalesList, gg>
      
      $guides
      <Guides[0] ggproto object>
      
      <empty>
      
      $mapping
      Aesthetic mapping: 
      * `x` -> `.data$time`
      * `y` -> `.data$model`
      
      $theme
      List of 136
       $ line                            :List of 6
        ..$ colour       : chr "black"
        ..$ linewidth    : num 0.5
        ..$ linetype     : num 1
        ..$ lineend      : chr "butt"
        ..$ arrow        : logi FALSE
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_line" "element"
       $ rect                            :List of 5
        ..$ fill         : chr "white"
        ..$ colour       : chr "black"
        ..$ linewidth    : num 0.5
        ..$ linetype     : num 1
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_rect" "element"
       $ text                            :List of 11
        ..$ family       : chr ""
        ..$ face         : chr "plain"
        ..$ colour       : chr "black"
        ..$ size         : num 11
        ..$ hjust        : num 0.5
        ..$ vjust        : num 0.5
        ..$ angle        : num 0
        ..$ lineheight   : num 0.9
        ..$ margin       : 'margin' num [1:4] 0points 0points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : logi FALSE
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ title                           : NULL
       $ aspect.ratio                    : NULL
       $ axis.title                      : NULL
       $ axis.title.x                    :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 1
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 2.75points 0points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.title.x.top                :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 0
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 0points 2.75points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.title.x.bottom             : NULL
       $ axis.title.y                    :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 1
        ..$ angle        : num 90
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 2.75points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.title.y.left               : NULL
       $ axis.title.y.right              :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 1
        ..$ angle        : num -90
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 0points 0points 2.75points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.text                       :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : chr "grey30"
        ..$ size         : 'rel' num 0.8
        ..$ hjust        : NULL
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : NULL
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.text.x                     :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 1
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 2.2points 0points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.text.x.top                 :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 0
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 0points 2.2points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.text.x.bottom              : NULL
       $ axis.text.y                     :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : num 1
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 2.2points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.text.y.left                : NULL
       $ axis.text.y.right               :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : num 0
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 0points 0points 2.2points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.text.theta                 : NULL
       $ axis.text.r                     :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : num 0.5
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 2.2points 0points 2.2points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.ticks                      :List of 6
        ..$ colour       : chr "grey20"
        ..$ linewidth    : NULL
        ..$ linetype     : NULL
        ..$ lineend      : NULL
        ..$ arrow        : logi FALSE
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_line" "element"
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
       $ axis.ticks.length               : 'simpleUnit' num 2.75points
        ..- attr(*, "unit")= int 8
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
       $ axis.line                       : list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ axis.line.x                     : NULL
       $ axis.line.x.top                 : NULL
       $ axis.line.x.bottom              : NULL
       $ axis.line.y                     : NULL
       $ axis.line.y.left                : NULL
       $ axis.line.y.right               : NULL
       $ axis.line.theta                 : NULL
       $ axis.line.r                     : NULL
       $ legend.background               :List of 5
        ..$ fill         : NULL
        ..$ colour       : logi NA
        ..$ linewidth    : NULL
        ..$ linetype     : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_rect" "element"
       $ legend.margin                   : 'margin' num [1:4] 5.5points 5.5points 5.5points 5.5points
        ..- attr(*, "unit")= int 8
       $ legend.spacing                  : 'simpleUnit' num 11points
        ..- attr(*, "unit")= int 8
       $ legend.spacing.x                : NULL
       $ legend.spacing.y                : NULL
       $ legend.key                      : NULL
       $ legend.key.size                 : 'simpleUnit' num 1.2lines
        ..- attr(*, "unit")= int 3
       $ legend.key.height               : NULL
       $ legend.key.width                : NULL
       $ legend.key.spacing              : 'simpleUnit' num 5.5points
        ..- attr(*, "unit")= int 8
       $ legend.key.spacing.x            : NULL
       $ legend.key.spacing.y            : NULL
       $ legend.frame                    : NULL
       $ legend.ticks                    : NULL
       $ legend.ticks.length             : 'rel' num 0.2
       $ legend.axis.line                : NULL
       $ legend.text                     :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : 'rel' num 0.8
        ..$ hjust        : NULL
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : NULL
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ legend.text.position            : NULL
       $ legend.title                    :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : num 0
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : NULL
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ legend.title.position           : NULL
       $ legend.position                 : chr "bottom"
       $ legend.position.inside          : NULL
       $ legend.direction                : NULL
       $ legend.byrow                    : NULL
       $ legend.justification            : chr "center"
       $ legend.justification.top        : NULL
       $ legend.justification.bottom     : NULL
       $ legend.justification.left       : NULL
       $ legend.justification.right      : NULL
       $ legend.justification.inside     : NULL
       $ legend.location                 : NULL
       $ legend.box                      : NULL
       $ legend.box.just                 : NULL
       $ legend.box.margin               : 'margin' num [1:4] 0cm 0cm 0cm 0cm
        ..- attr(*, "unit")= int 1
       $ legend.box.background           : list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ legend.box.spacing              : 'simpleUnit' num 11points
        ..- attr(*, "unit")= int 8
        [list output truncated]
       - attr(*, "class")= chr [1:2] "theme" "gg"
       - attr(*, "complete")= logi TRUE
       - attr(*, "validate")= logi TRUE
      
      $coordinates
      <ggproto object: Class CoordCartesian, Coord, gg>
          aspect: function
          backtransform_range: function
          clip: on
          default: TRUE
          distance: function
          expand: TRUE
          is_free: function
          is_linear: function
          labels: function
          limits: list
          modify_scales: function
          range: function
          render_axis_h: function
          render_axis_v: function
          render_bg: function
          render_fg: function
          setup_data: function
          setup_layout: function
          setup_panel_guides: function
          setup_panel_params: function
          setup_params: function
          train_panel_guides: function
          transform: function
          super:  <ggproto object: Class CoordCartesian, Coord, gg>
      
      $facet
      <ggproto object: Class FacetGrid, Facet, gg>
          compute_layout: function
          draw_back: function
          draw_front: function
          draw_labels: function
          draw_panels: function
          finish_data: function
          init_scales: function
          map_data: function
          params: list
          setup_data: function
          setup_params: function
          shrink: TRUE
          train_scales: function
          vars: function
          super:  <ggproto object: Class FacetGrid, Facet, gg>
      
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
      $labels$x
      NULL
      
      $labels$y
      NULL
      
      $labels$title
      NULL
      
      $labels$fill
      [1] "effect"
      
      

