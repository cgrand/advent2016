(ns advent2016.day1-5
  (:require [clojure.edn :as edn]))

;; day1
(def day1-input "R4, R3, R5, L3, L5, R2, L2, R5, L2, R5, R5, R5, R1, R3, L2, L2, L1, R5, L3, R1, L2, R1, L3, L5, L1, R3, L4, R2, R4, L3, L1, R4, L4, R3, L5, L3, R188, R4, L1, R48, L5, R4, R71, R3, L2, R188, L3, R2, L3, R3, L5, L1, R1, L2, L4, L2, R5, L3, R3, R3, R4, L3, L4, R5, L4, L4, R3, R4, L4, R1, L3, L1, L1, R4, R1, L4, R1, L1, L3, R2, L2, R2, L1, R5, R3, R4, L5, R2, R5, L5, R1, R2, L1, L3, R3, R1, R3, L4, R4, L4, L1, R1, L2, L2, L4, R1, L3, R4, L2, R3, L1, L5, R4, R5, R2, R5, R1, R5, R1, R3, L3, L2, L2, L5, R2, L2, R5, R5, L2, R3, L5, R5, L2, R4, R2, L1, R3, L5, R3, R2, R5, L1, R3, L2, R2, R1")

(defn day1-1 [input]
  (let [[[x y]] (reduce (fn [[[x y] [dx dy]] [_ dir n]]
                          (let [n (Long/parseLong n)
                                [dx dy] (case dir
                                          "R" [dy (- dx)]
                                          "L" [(- dy) dx])]
                            [[(+ x (* n dx)) (+ y (* n dy))] [dx dy]]))
                  [[0 0] [0 1]]
                  (re-seq #"([RL])(\d+)" input))]
    (+ (Math/abs x) (Math/abs y))))

(defn day1-2 [input]
  (let [locations (mapcat first (reductions (fn [[locs [dx dy]] [_ dir n]]
                                              (let [[x y] (peek locs)
                                                    n (Long/parseLong n)
                                                    [dx dy] (case dir
                                                              "R" [dy (- dx)]
                                                              "L" [(- dy) dx])]
                                                [(into [] (map (fn [n] [(+ x (* n dx)) (+ y (* n dy))])) (range 1 (inc n))) [dx dy]]))
                                  [[[0 0]] [0 1]]
                                  (re-seq #"([RL])(\d+)" input)))
        [x y] (reduce (fn [visited loc]
                        (if (visited loc) (reduced loc) (conj visited loc)))
                #{} locations)]
    (+ (Math/abs x) (Math/abs y))))

(def day2-input "LUULRUULULLUDUDULDLUDDDLRURUDLRRDRDULRDDULLLRULLLURDDLRDLUUDDRURDDRDDDDRDULULLLLURDDLLRLUUDDDRLRRRDURLDDLRRLDUDRRRDLDLRRDLDLUURRLRULLULRUDRDLRUURLDRDLRLDULLLUDRDDRLURLUUDRLLLDRUUULLUULRUDDUDRDUURRRUDRLDDUURDUURUDRDDLULDDUDUDRRDDULUDULRDRULRLRLURURDULRUULLRDDDDRRUUDDDUUDRLLRUDRLRDLRRLULRLULRUDDULRLLLURLDDRLDDLRRLDRDDDRRLRUDRULUUDUURLDLRRULUDRDULDLLRRURRDDLRRRLULUDUUDDUDDLRDLRDRLRLDUDUDDUDLURRUURDRLRURLURRRLRLRRUDDUDDLUDRLUURUUDUUDDULRRLUUUDRLRLLUR
LDLLRRLDULDDRDDLULRRRDDUDUDRRLLRUUULRUDLLRRDDRRLDDURUUDLUDRRLDURDDRUDLUDUUDLDLLLDLLLDRLLDLRUULULLUUDULDUUULDDLRUDLLUDLUUULDRLUDRULUUDLDURDLDUULLRDUDRDLURULDLUUUDURLDDRLLDRLRDDDUDRUULLDLUDRRDDLDLUURUDDLDRURRLULUDDURLDRDRDUDDRRULRLDURULULRURDUURRUDRDDRDRLDRDUUDLRULRDDDULRURUDRUUULUUDDLRRDDDUDRLRUDRDLRRUDLUDRULDDUDLRLDDLDRLRDLULRDRULRLLRLUDUURULLLDDUULUUDDDUDRRULDDDULRUDRRLRLLLUDLULDUUULDDULDUUDLUULRDLDUDRUDLLDLDLLULDDDDLUDDUDRUDLRRRDDDDDLLRRDRUUDDDRRULRUDUUDRULLDLLLDDRDDUURLUUURUDRUDURLRUUUULUUURDRRRULDUULDLDDDRDDDDLLDRUDRDURLDDURDURULDDRLLRRLDUDRDURRLDRDLLULUUUD
LDDLRLRDDRLRUDDRDDUDRULUUULULDULRUULLRRDUULRDUUDDDRRULDDUDRLLLDULURDLDDRLLRURULULDLDULRDLDLRULUDLLDRUDLDURRDULDDRLRURDLLUDRDDDUDLUDULURULRDRLRULDLLRLDRRUDRDRUDRLDLRLUUURURRRLDDULLULLLRLRLULDLLRLDDRLDULURULRUURRUUURRUDRLRRURURDDDRULDULDLDLRRRLLDDRRURRULULULDRDULDRRULDUDRRLDULDRDURRDULLRRRLLLLRRLLRRRDRURDUULLURURURDDRRDRLLLULRRRDRLDRLDRDLLRUUDURRDRRDLLUDLDRLRLDLUDRDULRULRRLLRDLULDRLUDUUULLDRULDDLLRDUUUDRUUUUULUURDDLLDUURURRURLLURRDDUDUDRUUDDRDDRRLRLULRLRRRDRLLRRLLLDUULLUUDDLULLLDURRLLDRLDRDRLRRLRRULRRRRLRRRRRURUDULUULRDLLDRLRRDUURDRRUDRURRRDDRLDDLRLUDRDRDRRLDDDRDDRRRDUDULRURRDRDLLDRUD
UUUDLDDLRDLLLLRUUURDDLLURRUUURLUULLURUUDUDLDULULLRRRRLLLRDLLUDRUURDRURUDRURRLRLDRURLUDRLULRRURDDDURLLDULDLRRRDUUDDDRDLRUURRDRDRLRDLULRLDDRULRULDRDUDRUURLDLUDDULLLRURRLURLULDRRLUUURURLDLDDULLLRUUURDDDUURULULLUUUDUDRLLRRULUULDDDLLUDLURLLLRRULLURDRLUUDDLLDLLLUDULLRDRRRURDRUDUDUULUDURDLRUDLLRDDRURUDURLRULURDDURULLRDDRLRRDRLLULRDDDULRDLRULDDLRRDULDLUURRURUULRRDUURUDRRRRRLDULDLRURRULULDLRDDDRLLDURRULDUDUDRRRLUULRLUDURRRLRLDURRRRUULDRLUDDDUDURLURUDLLUDRDDDRLLURLRLDDURUUDDDUDUR
RURRRRURUDDRLURUDULRDUDDDUURULDRRRRURDLDRRLLDLUDLRRLRRUULLURULLRDLLRDDDDULLRLLDDLLRUDDULDUDLDURLRUULDDURURDURDLDRRULRURRRRRLRRLLUDURRURULRLRDLRLRRRLLURURDLLLDLDDULDLUDDLLLRUDDRDRLRUDRRLDDLRDLRLRLRLRRDUUURRUDRRLDLRRUULULLUDRRRUDLURDRUULDRDRRLUULULDDLURRLDULLURLDRLDULDRLLDLUUULLULRRDDRURRURLDLDRRLLLLLUDUURUULURLRDDDLRRRRLLLURUDLDDRDDRRUDURUULDRRULLLRRLRULLLRLDDLLRRLRURLRDRUDULLDDLDDDDDLDURURDLULRDDLRDLLRURLLRDLRUDDRDRRDURDURLUDRLDUDDDRRURRLUULURULLRLRDLRRLRURULLDDURLLRRRUDDRDLULURRRUUUULUULRRLLDLRUUURLLURLUURRLRL")

(defn move [button direction]
  (case direction
    \L (if (not= (mod button 3) 1) (dec button) button)
    \R (if (not= (mod button 3) 0) (inc button) button)
    \U (if (<= button 3) button (- button 3))
    \D (if (>= button 7) button (+ button 3))))

(defn day2-1 [input]
  (apply str (next (reductions #(reduce move %1 %2) 5 (clojure.string/split-lines day2-input)))))

(def vicious-keypad "  1  
 234 
56789
 ABC 
  D  ")

(defn move2 [idx direction]
  (let [idx' (case direction
               \L (if (not= (mod idx 6) 0) (dec idx) idx)
               \R (if (not= (mod idx 6) 4) (inc idx) idx)
               \U (- idx 6)
               \D (+ idx 6))]
    (if (= \space (nth vicious-keypad idx' \space))
      idx
      idx')))

(defn day2-2 [input]
  (apply str (map #(nth vicious-keypad %) (next (reductions #(reduce move2 %1 %2) (.indexOf vicious-keypad "5") (clojure.string/split-lines day2-input))))))

(def day3-input
  "  810  679   10
  783  255  616
  545  626  626
   84  910  149
  607  425  901
  556  616  883
  938  900  621
  638  749  188
  981  415  634
  680  557  571
  523  604  270
  910  954  484
  464  392  514
  458   52  687
  696  438  832
  213  583  966
  572  571  922
  451   42  686
  177  390  688
  151  136  705
   92  413  191
  789  676  377
  486  262  600
  450  708  472
  556    9  481
  157   85   94
  574   93  549
  539  165  487
  815  742   73
  353  773  428
  526  152  680
  433  711  557
  168  632  306
  848  992  757
  885  786  890
  469  475  146
  899  833  137
  864  202  688
  101  902  620
  529  937  826
   41  381  521
  562  883  804
  468  197  272
  451    8  420
  561  193  630
  597  951  383
  171  845  251
  541  810  157
  268   46  712
  332    2  397
  100   47  436
  194  665  205
  325  277   21
  170  652  205
  765  165  506
   15  257  144
  762  124  401
  662  543  531
   29  425  308
  667  785  299
  935  758  405
  504  998  367
  771  947  630
  490  933  978
  441  498  896
  862  896  607
  655  935  194
  286  240  324
  368  723  311
  419  762  600
  316  903  529
  197  215  215
  551  461   77
  855  318    7
  894  690   86
  451  648  416
  608  132  385
  420  761  112
  560  711  195
  371  750  506
  188  307  584
   26  377  622
  304  701  292
  286  630  642
  883  880  379
  774  564  597
  300  692  701
  529  595   27
  740   76  445
  567  648  422
  340  163  901
  374  775  902
  308  827  882
  529  371  374
  996  587  162
  534  360  516
  924  160  276
  724  896  687
  929  971  578
  798  252  761
  512  991  812
  465  758   49
  724  446  571
  482  196  544
  553  247   86
  624  552  778
   73  143  127
  556  471  749
  224  927  383
  133  636  847
  174  985  569
  572  819  881
  282  818  383
  535  429  780
  953  540  815
  577  302  494
  530  654  370
  670  739  168
  700  695  806
  196   48  928
  255  805  749
   65   96  969
  292  860  929
  556  269  297
   43  832  407
  542  723  438
  919  139  407
  709  194  955
  847  237  933
  321   41  216
  778  749  374
  782  745  529
  716  572  251
   90   49  976
  639  557  740
  148  125  784
  143  819  382
   71  729  563
  309  500  806
   25  412  594
  296  600  237
  681  187  142
  758  913  288
  163  972  266
  197  352  190
  383  190  562
  206  214  393
  566  307  294
    2  284  335
  564  472  394
  635  928  589
  169  744  574
  710  386  589
  970  386  827
  943  424  134
  846  269  712
  266  765  615
  344  824  685
  250  222  554
  377  586  859
  398  526  275
  317  996  937
  503  364  389
  212  782  533
  584  539  589
  731  200  584
  773  389  578
   43  482  104
  432  140  339
  193  758  673
  612  882  582
  314  920  130
  522   40   26
  695  939  149
  955  121  552
  728  850  661
  524  766  433
  817  221  992
  753  580  543
   72  392  873
  445  897    3
  144  508  567
  354  990  566
  477  392  687
  602  846  520
  321  577  677
  716  518   55
  367   77  545
  361  473  504
   98  893  887
  854  920  887
  860  174   30
  389  857  797
  686  968  907
  613  275  595
  855  440  906
  749  494  735
  527  895  550
  767  971  488
  118  814  148
  854  193  480
  847  425  378
  697  159  357
  282  476   48
   96  314  176
  949  597  903
  956  478  885
  714  754  278
  757  547  210
   53  223  170
  355  725  928
  930  780  762
  924  581  266
  570  132  283
  625  674  529
  159  719  325
  316  670  929
   55  655  542
  344   19  791
  437  805  312
  327  867  647
  521  405  496
  383   58  117
  638   36  175
  924   59  112
  401   66  353
  740  785  823
  713  725  622
  821  702  246
  378   24  958
  690  718  924
  486  788  537
  377  214  670
  514  720  427
  451  927  877
  808  868  872
  554   94    2
  534  516  715
  735  318  125
  880  496  755
  724  115  567
   23  105   89
  725   55  561
  599   44  581
  378  661  173
  628  640  632
  747  817  448
  557  248  338
  743  833  776
  309  895  759
   18  696  851
  328  775  356
  220   37  499
  865  390  651
  736  397  205
  645  949  170
  638  860  143
   23  262   98
  822   46  842
  663  687  860
  941  700  745
  762  304  509
  154  275  369
  728  155  324
   99  113  485
  245   82   62
  294   76  484
  215  664  398
  146  336  461
  102  591  503
  535  814  749
  250  410  892
  672  467  212
  304  108  285
  300  246   11
    4  304  284
  115  132  112
  460  334  739
  453  281  792
  505  591    6
  482  413  975
   26  763  980
  226  377  727
  406   59   39
  570  325  691
  333  438  966
  267  792  229
  130  384  854
  375  165  187
   37  498  403
  357  509  242
  710  796  296
  708  187  265
   46  762  279
   84  589  760
  578   38  226
  624  558  570
  338  517  276
  547  498  648
  626  265  677
  144  662  193
  581  820  407
  477  567  232
  582  890  926
  167  458  502
  635  841  607
  505  346  239
  522  970  506
  608  830  686
  100   89  353
   95  159  652
   24  163  786
  328  313  534
  793   52  249
  750  274  683
  885  463  247
  534  326  391
  938  726  199
  893  620  120
  899  410  508
  226  896  459
  677  694  780
  880   15  831
  909  683  903
   55    7  541
  294  221  109
  286  216  507
  239  652  380
  948  760  431
  772  258  275
  562  226  631
  503  264  765
  690   42  369
  761  541  373
  232  596   75
  925   60  402
  550  181   16
  600  579  701
   92  419  696
   26  117  290
    4  487  157
   21  474  308
   99  827  835
  279  216  451
  267  739  749
  309  456  262
  320   91  282
   52  431  304
  773  784  932
  474  483  932
  703  975  257
  851  227  584
   17  224  365
  845   96  536
  258  150  905
  797  119  876
  862  196  220
  954  964  355
  534  979  302
  905  509  628
  153  185  273
  169  538  509
   43  477  356
  702  357  940
  340  403  284
  638   86  744
  329  426  903
  222  720  682
  127  624  253
   28  849  485
  555  158  599
  553  690  443
  598  926  185
  611  934  868
  986    8  983
  166  396  946
  500  822  662
  507  715  828
  294  790  587
  661  779  235
  549  594  657
  771  918  800
  923  896  983
  866  203  437
  723  465  852
  589  717  731
  332  331  710
  984  484  794
  750  479  886
  857    5  286
  400  841   63
  665  513  508
  841  739  513
  331  586  669
  420  561  690
  346  104   22
  847  758  149
  570  211  816
  524  868  962
  483  229  317
  408  555  325
  682  650  285
  646  987  974
  467  368  779
  442  640  968
  644  131  184
  903  916  162
  565  890   91
  474  763  351
  569  178  709
  520  618  666
  437   75  213
  509  471  758
  298  486  904
  364  416  429
  513  971  271
  169  863  202
   15  206  565
  163   69  713
  167  186  542
  908  550   89
  936  764  451
  118  467  464
   89  385  375
  179  165  545
  143  514  187
  313   47  636
  477  830  550
  769  808  577
   74  756  630
  698  799  654
  721  387   36
  993  763  945
  707  746    7
  955  113  948
  723  532  526
  174  795  204
  671  968  575
  523  256  109
  570  186  296
  350  351  215
  141  251   22
  532  217  695
  460   37  719
  695   69  516
   36  597  350
  670  552  556
  287  143   35
  400  801   45
  133  921   71
  637  169  646
  108  721  890
  655  681  311
  885  393  603
  375  388  113
  976  522  534
   15  516  627
  685  602  535
  669  390  781
  845  950  348
  388   30  379
  825  955   46
  360  579  898
  363  573  660
   33   30  864
  905  723  916
  968  648  655
  178  181  363
  754  262  268
  883  837   45
  216  687  222
  520  973  909
  808  968  943
  335    3  202
  211  605  517
   32  298  358
  184  488  173
  741   23  328
  400  482  144
  626  491  451
  920  546  219
  363  734  861
  739  417  685
  954  470  541
  598  679  950
  550  372  450
  980  459  213
  353  374  293
  720  220  256
  173   29  571
  289  769  833
  372  793  345
  578  298  332
  763  225  167
  258  519  307
  504    7  649
  186  319  883
  358  322  918
  293   60  330
  373  562  550
  310  532  573
  741  129  533
  701  614  869
   54  736  587
  451  131  817
  499  784  651
  931  681  193
  674  311  500
  900  312  197
  553   94  331
    9  715  572
  590   97  275
  579  713  299
   20  345  741
  817  738  534
  819  963  497
  168  303  997
  462  599  698
  400  772  485
  755  922  928
  591  847  180
  500  135  977
  946  940  751
  658  368  790
  720  714  141
  850  261  594
  615  116  476
  660  156  488
  485  895  378
  797  992  614
  847  652  838
  842  516  364
  745  444  329
  175  362   84
  684  223  578
   43  291  394
  702  222  862
  208  247  494
  601  236  234
  780   53  675
  754  135  126
   26  776   52
  735  716  136
  591  829  171
  606  373  824
   51  926  766
  273  161  558
  215  557  149
  393  703  653
  318  208  207
  891   54  570
  790  153  689
  521  693  423
  559  986  542
   58  611  404
  178  509  602
  684  120  975
  791  407  811
   94  321   66
   14  317  266
  108   14  271
  580  454  391
  781   82  849
  419  406  775
  396  298  237
  448  375  330
  747  301  322
  103  835  120
  138  897  630
  127  102  546
  518  552  412
  398  442   43
  586  972  380
   30  535   91
   42  384  962
   61  414  942
  610  147   65
  945  155  418
  667   54  375
  473  251  187
  440  222  124
  886  158  163
  862  493  149
  805  451  536
   59  108  458
  663  613  719
  264  525  574
  755  176  168
  390    6  783
   50  561  233
  401  568  582
  121  979  769
   94   77  830
  195  938  201
  124  626  161
  668  633   35
  662   29  164
  394  658  768
  203  918  850
  466  425  399
  353  804  714
  323  851  640
  152  939  642
   29  309  484
  579  529  822
  608  262  731
   38  756  450
  433  828  740
  431  895  693
  392  477  399
   25  925  513
  368  969  491
  671  736  911
  307  198  660
  662  859  311
  853  596  526
  917   24  461
  677  574  960
  697  220   90
  203  458  102
  499  284   29
  400   79  582
  484  195  597
  575  276  912
  493  269  347
   23  593  223
  476  802  358
   33  944  255
  715  117  460
  739  885  586
  748  954  527
  734  773  643
  542  202  117
   15  976  460
  309  830  331
  319  208  557
  458  822  461
  545  784  690
  878  372  858
   57  295  470
  268  537  822
  271  301  699
  806  909  878
  744  182  571
  106  895  468
  121  778   28
  641  202  593
  710  724  592
  125  784  603
  654  771   83
  721   87  543
  585  724   89
  381  739  524
  623   28  494
  869  729  292
  228  736  298
  803   10   95
  700  224  786
  738  512    9
  708  407  775
  558  645  863
   45  209  466
  540  809  587
  372  512  717
  416  203  974
  272  496  928
  816  141  903
  675  894   84
  567  900  957
  827  122  189
  882  860   56
   98  792  196
  861  461  209
  685  339   87
  585  464  235
  640  156  703
  817  596  321
  893  462  996
  679  536  208
  199  455  365
  873  260  492
  528  179  563
  689  563  849
  887  417  507
   64  270  198
  595  214  166
  566  232  242
  921  102  212
  187  202  335
  992  169  475
  736  754  200
  655  374  127
   84  492  193
   21  709  972
  199  208  236
  216  683  926
  479  669  604
  437  872  293
  789  256  515
  341  948  637
  142  933  536
  207   82  218
  702  249  779
  253  369  874
  508  255  254
   91  536  541
  212  813   28
  144  406  563
  180  513  277
  421  842  639
  570  520  522
  224  830  592
  153  582  606
   81  415  239
  160  553  735
  525  348  778
  454  352  626
  609  460  169
  559   57  334
  784  428  242
  706  867  289
  637  914  281
  620  407   83
  152  446   90
  260  331  799
  301  677  725
  708  254  328
  418  147  798
  732  344  963
  627  626  302
  670  241   76
  220  383  376
  733  124   50
  795  673  466
  136  637  423
  823  258  700
  204  936  878
  730  976  981
  272  310  894
  333  201  863
   90  122  621
   90  811  209
  275  904  283
  193  125  189
  127  961  283
  347  529  829
  352  738  734
  878  726  411
  942   54   34
  429  750  426
  367  938  424
  501  447  757
  566  773  648
  382  140  899
  462  353   90
  230  493  945
  425  290  415
  894  360   21
  897  529  431
  914  124  338
   78  766  876
  858  664  764
  598  664  317
  630  548  772
   30  483  604
  642  331  545
  518  702  474
  546  750  887
  252  663  547
  813  917  671
  852  367  894
   97  192  265
  661  587  858
  726  674  748
  578  178  878
  327  535  608
  426  419  871
  559  837  229
  851  721  708
  860  978  770
  308  604  626
  198  168  408
  138  628  799
  669  525  918
  804  762  652
  389  429  554
  618  566  360
  814  648  887
  677  697  659
  600  660  162
  256  749  195
  840  734  216
  445  192  960
  341  226  975
  699  140  114
  763  833  533
  234  835   38
  798   10  569
  190  745  418
  183  563  486
  295  224  197
  437  724  885
  197  706  328
  268  709  702
  351  679  694
  642  555  769
  333  521  883
  182  532  772
  517  543  711
  657  154  169
  134  888  300
  217  121  209
  346  796  100
  755  681  817
  277  733  980
  677  162  481
  527  191  433
  293  999  653
  429  850  503
  562  205  402
  217  323  414
  565  402   43
  730  223  537
    4  701  567
  737  570  523
  644  510  459
  390  252  367
  344  715  179
   62  236  586
  527  310  137
  526   96  548
  585  357  407
  768  532  384
  591  421   43
  928  129  533
  228  469  848
  886  349  596
  392  231  867
  507  664  870
  546  881  121
   28  306  275
  688  284  261
  683  495   31
  733  191  899
   83  785  730
  738  668  220
  795   69  237
  148  175  238
  872  139  100
  673  671  744
  222  421  346
  824  971  589
  283  135  474
  626   48  487
  426  172  548
  796  463  616
  547  349  568
  717  798  428
  248  977  192
  337  683  128
  480  487  231
  817  559  882
  413  935  879
  694  724  447
  221  458  449
  649  523  725
  689  131  311
  726  707  273
  712  689  127
   65  338  183
  612  523  679
  631  834  297
  701  320  433
  265  518  602
  691  519  160
  463    4  575
  777  590  394
  790  975  201
   22  449  242
  578  308  911
  371  157  191
  489  263  789
  962  696  390
  494  760  494
  760  656  350
   57  322  551
  639  105  616
  676  402  236
  269  464  893
  265  573  312
  472  822  682
  410  385  584
  882   56  493
  596  330  827
  184  494  873
   61  580  793
  157  260  128
  440  239  390
  701  174  230
  946  357  394
  273  423  258
  529  438  733
  552   75  892
  946  755  996
   64  836  112
  971  192  928
  188  378  692
  179  299  676
   91  177  202
  748  644  634
  551  355  345
  265  504  410
  644   58  450
  103  716  556
  691  679  128
  166  255  174
  415  682  368
  474  862  434
  348  462  133
  704  626  374
  979  835  426
  239  897  288
  381  953  234
  181   65  504
   61  803  297
  761   22  946
  771  822  908
  900  914  563
  656  948  114
  349  202  594
  322  294  811
  535  484  837
  532  438  869
  700   94  814
  691  557  159
  201  512  738
  598  652  742
  269  642  772
  698   23   49
  376  375  689
  375  476  819
  426  421  559
  683  775  420
  876  374  995
  281  556  587
  990  137  273
  782  928  299
  895  829   65
  228  687  764
   62  496  905
  210  277  352
  732  461  535
  418  364  561
  958  373  189
  640  617   27
  185  680  698
  697  507  688
  324  836  143
  434  868  658
  342  516  628
  351  760  280
  796  663  876
  977  133  813
  169  326  101
  139  575  796
  236  597  851
  191  704  375
  568  733  436
  615   68  728
  478  768  617
  531  594  596
  898  898   64
  596  181  707
  371  381  259
  609  406  528
  810  271  308
  211  975  596
  963  896  551
   94  362  418
  812  351  848
  732  495  708
  866  246  209
  973  682  792
  898  535  672
  667  237  783
  325  642  229
  419  654  754
  328  374    7
  359  468   93
   91  453   93
  923  741   53
  721  938  589
  235  716  605
  466  387  199
  554  430  681
  166  181  864
  699  998  953
  999  962  718
  330  124  822
  443  536  930
  293  631  674
  197  574  315
  407  183  293
  432  417  537
   31  571  657
  901  555  463
  686  456  465
  217  259    3
  742  535  427
  881  347  555
  769  659  299
  134  577   20
  252  566  877
  181   10  885
  191  829  994
  744  649  867
  910  354  781
   68  767  930
   88  716  850
   22  290  121
  226  212  666
  266  327  812
  356  112  148
  252  397  741
  325  674  834
  389  442  946
  898   83  618
   51  807  862
  844  772  461
  831  546  467
  644  476  539
  758  758  722
  346  512  463
  157  427  697
  439  672  243
  192  869  150
  890  977  753
  962  767  607
  818  926  500
  960  927  219
  377    9  389
  661  191  869
  695  149  368
  358  342  778
  474  396  202
  546  585  853
   74  281  734
  830  295  611
   19  813  388
  847  963  378
   78  140  278
  531  580  246
  550  546  415
  739  419  197
  803  266  247
  285  672  123
  669   51  665
  525  662    5
  998  619  667
  737  368  910
  533  550  245
  899  667  932
   80  302  566
  508    1  576
  454  303   15
  752  463  159
  119  380  906
  702  279  942
  234  198  326
  262  207  305
  214  388   64
  975  779  523
  975  243  519
  694  895   79
  750  477  112
  746  470  108
  201  299  119
  748  890  652
  808  897  387
  908  617  466
  739  750  302
  887  765  558
  464   97  662
   11  745  109
  454  537   27
  446  363  118
  265   33  670
  862  497  147
  681  488  582
  370  131  389
  645  652  560
  496  548  779
  910  434  642
  793  105  303
  232  468  916
  932    5  657
  782  634  626
  429  642  326
  946  618  408
  760  711  553
  561  391  385
  614  834  961
  585  853  375
  188  562  635
  775  758  496
  300  128  476
  747  817  333
  288  608  259
  410  883  700
  142  691  562
  222  270  870
  654  341  896
  548  133  474
   49  712  796
  486  607  561
  483  920  970
  510  553  658
  876  682  369
  654  744  670
  508  888  671
  648  111  694
  213  954  529
  548  879  258
  342   15  155
  265  880  313
  613   36  583
  285  774  605
  696  776  742
  772  230  561
  239  304  710
  602  387  940
  871  107  512
  182  321  376
  927  392  527
  677  124  195
  312  270  938
  755  308  986
  400  779  601
  876  843  690
  964  719  119
  925  665  237
  730  719  310
  352   86  123
  583  801  629
  697  340  198
  150  635  446
  905  183  133
  648  654  298
  445  743  383
  483  628  344
  460  822   64
  264  872  384
  496  291  691
  130  742  608
  491  590  986
  737  317  602
  442  179  684
  617  256  642
  711  688  915
  679  804   29
  127  869  890
  621  677  347
  306  486  533
  645  198  481
  706  855  997
  686  743  117
  152  947  939
  271  251  352
  324  621   83
  562  745  349
  901  797  273
    7   84  696
  895  857  751
  692  663  805
  692  489  122
  876  848  930
  667  851  155
  226  218  502
  447  876  635
  395   40  430
  652  999  312
  362  992  135
  714  360  668
  603  393  858
  176   36  470
  956  803  884
  678  829  391
  340  128  810
  643  777  545
   71  314  335
  705  667  881
  119  708  664
  480  524  560
  432  183  165
  983  946  881
  788  472  442
  386  767  510
  864  823  566
  764  684  955
  155  309  725
  459  300  826
  627   85  796
  497  376  448
  827  969  784
  408  875  120
  764  883  698
   81  590  675
  128  549  653
  127  606  712
  668  989  706
  776  440  615
  121  840  169
  641  648  803
  224  671  825
  733  419  107
   86  208  359
  383  809  426
  322  741  122
  772   75  577
  844  100  782
  128  139  344
  702  420  230
  311  488  724
  633  209  661
   33  564  249
  459  120  886
  493  473  761
  252  719  939
  506  628  748
  673  843  501
  124   54  798
  421  761  726
  521  732   70
  395  438  839
  600  434  851
  464  374   29
  598  900  349
  817  637  266
  558  625  311
  503  806  254
  527  415  447
  131  972  675
  816   36  481
  870  880  637
  215  908  266
  973   18  622
  973  940  514
  463  923  875
  472  982  282
  868  808  269
  544  272  456
  961  836   90
  130  888  215
  974  276  275
  309  233  253
  973   46  438
  842  277  438
  366   80  179
  419  901  846
   82  907  966
  596  354  513
  381  362  490
  846   11  884
   22  718  970
  396  766  862
  397   62  598
  222  158  646
  814  712  225
  732  629  623
  809  626  692
  979  632  811
  503  139  372
  462  517  811
  256  899  609
  216  570  483
  902  733  385
   89  928    4
  887  695  386
   35  568  155
  781   58  203
  775  604  291
  367  692  689
  101  158  677
  336  580  368
  981  337  174
  900  880  593
  275  613  463
  311  907  363
  368   83  832
   64  974  980
  157  562  421
   12  820  590
  160  464  322
  245  444  382
    9  312  134
  257  306  288
  237  449  297
  142  600  661
  320  363  821
  721   84   89
  589  509  116
  413  594  181
  890  477  712
  742   65  245
  229  432  917
  536  189  821
  732  401  407
  515  210  512
  733  778    2
  852  451  210
  130  360  208
  230  408  748
  667  499   94
  467  112  789
  649  764  715
  253  908   53
  775  878  673
  265    5   24
  717  434   72
  687  428   72
  268  436  903
  678  450  742
  636   40  792
  555  104  649
  538  608  340
  370  525  847
  555  830  585
  763   92  375
  754  898  314
  153  560  139
  224  663  666
  138  344  595
  278  448  532
  413  492  470
  432   98  335
  148  795  903
  729  903  101
  818  186  960
  853  631  290
  761  170  666
  171  582  732
  189  731  633
  779   20  287
  883  726  449
  701  139  747
  571   29  567
  918  166  232
   98  356  853
  815  512  449
  911  504  671
  728  414  257
  515  517  657
  590  854  517
  388  526  831
  646  217  989
  845  355  289
  573  306  156
  563   11  456
  107  320  601
   37  287  714
  167  290  958
  198   37  287
  896  491  695
  712  282  239
  223  252  604
  524  955  584
  883  890  665
  818  817  242
  518  236  632
  410  222  191
  310  135  666
  983  634  348
  671  476  306
  986  665  111
  109  220  399
  717  738  695
  764  825  534
  616  315  977
  628  142  873
   19  287  155
  967  255  868
  191   80  844
  986  220  988
  419  521  444
  454  916  489
   71  859  500
  897  459  731
  823  791  216
  351  677  556
  840  208  612
  983  156   22
  988  318  633
  472  628  495
  341  608  343
  771  779  528
  818  149  422
  598   52  436
  678  130  285
  455  502  177
  461  245   81
  466  382  258
  181  661   64
  808  499   22
  892  243   76
  341  643  531
  717  328  856
  811  779  683
  666  220  797
  613  453  417
  978  632  462
  457  620  387
  558  681  351
  105  337  432
  880   55  818
  438   63  136
  709  100  700
  229  792  280
  427  985   53
  442  385  325
  918  328  642
  754  291  642
  970   74  973
  296   55  952
  577  458  924
  645  507  523
  589  149    6
  491  933  297
  871  822  303
  436  938  577
   98  762  322
  368  875  708
  607  636  385
  488  362  722
  642  379  510
  271   30  954
  338  296  210
  125  279  887
  614  178  645
  268  237  471
  578   60  720
  776  691  995
  814  565  784
   58  358  474
  968  573  398
  358  613  323
  851  694  665
  109    4  181
  366  741  777
  447  747  870
  738  460  241
  905  694  448
  440  901  565
  293  278  940
  822  276  877
  746    2  338
  227  915   30
  604  733  486
  501  359  493
  536   79  751
  621  623  135
  524  547  812
  917   11  982
  505   55  826
  580   55  287
  228  805  345
  586  101  202
  624  829  465
  262  645  636
  942  775  496
  724  942  398
  803  499   16
  326  565  969
  751  977  964
  320  725  153
  258  772  689
  107  421  839
  402  399  578
  116  927  560
  508  685  100
  970  581  680
  119   98  451
  904  580  314
  207  186  373
  791  286   21
  917  199  388
  210  549  203
  212  270  266
    2  429  355
  297  647  659
  233  537  895
  142  284  332
  219  237  361
  246  247  401
  288   81  328
  360  346  279
   21  262  298
  343  211   50
  637  778  813
  820  240   32
  660  781  805
  638  470  759
  779  198  372
  158  392  433
    5  274  133
  189  346  169
  194   74   37
   13  767  447
  167  546  364
  176  618  336
  554  638  712
  615  663  776
  824   62  142
  582  320  499
  302  278  545
  751  296   71
  366   35  493
  196  657  381
  364  685  134
  888  756  128
   17  799  479
  872  685  363
  879  279  556
  665  164   40
  264  418  539
  627  575  589
  978  792  584
  662  693    9
  988  838  552
  870  299   11
  141  674  546
  460  912  693
  216  795  292
  531  699  441
  207  795  373
  719  461  831
  571  491  664
  142  282   59
   48   89  556
  147  278  506
  334  990  607
  483   42  370
  766  978  303
  343  336  215
  283  745  857
  306  587  642
  566  764  323
  372  267  609
  878  505  315
  282  877  342
  283  369  682
    4  823  926
  339  831  891
  521   33  942
  704  816  318
  416  621  503
  163  684  625
  514  141  646
  362   81  368
  134  819  425
  324  768  190
  985  309  356
   41  491  802
  997  793  905
  976  684  837
  368  954  863
  878  407   43
  216  662  557
   82  425  547
  286  486   43
  841  595  727
  809  169  417
  233  566  654
  547  419  783
   91  422  981
  628    1  945
   83  747  306
  399  806  592
  346  708  392
  813  865  624
  516  636   29
  592  753  610
  440  460  145
  457  457  114
   40   19  165
  494  659  248
  647  950  224
  810  965  241
  913  630  245
  919  652  409
   38  151  355
  430  239   96
  372  597  360
  711  494  370
  176  710  108
  130  230  503
  188  509  421
  850  394  702
   68  744  665
  919  923  873")

(defn day3-1 [input]
  (->> input
    (re-seq #"\d+")
    (map edn/read-string)
    (partition 3)
    (map sort)
    (filter (fn [[a b c]] (< c (+ a b))))
    count))

(defn day3-2 [input]
  (->> input
    (re-seq #"\d+")
    (map edn/read-string)
    (partition 3)
    (partition 3)
    (mapcat #(apply map vector %))
    (map sort)
    (filter (fn [[a b c]] (< c (+ a b))))
    count))

(def day4-input "fubrjhqlf-edvnhw-dftxlvlwlrq-803[wjvzd]
kzgwomvqk-rmttgjmiv-lmxizbumvb-902[zmnji]
dkqjcbctfqwu-dwppa-fgukip-596[syiua]
xjinphzm-bmvyz-ytz-gjbdnodxn-135[nzbdj]
uwtojhynqj-hfsid-xytwflj-177[ztsqu]
udpsdjlqj-fkrfrodwh-ilqdqflqj-491[uscwt]
kdijqrbu-fbqijys-whqii-sedjqydcudj-790[dijqb]
udpsdjlqj-hjj-uhdftxlvlwlrq-439[jldhq]
bnmrtldq-fqzcd-bqxnfdmhb-bgnbnkzsd-zmzkxrhr-105[bdnzm]
lejkrscv-wlqqp-sleep-ivrthlzjzkzfe-789[elzjk]
zlilocri-ciltbo-obxznrfpfqflk-419[spmzt]
tyepcyletzylw-nsznzwlep-qtylyntyr-821[shmzu]
ynssr-vtgwr-lmhktzx-865[kyqlr]
crwwv-pzxsbkdbo-erkq-pxibp-991[bpkrw]
uiovmbqk-ziuxioqvo-zijjqb-bmkpvwtwog-616[sizek]
qfmcusbwq-foppwh-cdsfohwcbg-194[cfwbh]
nvrgfezqvu-irsszk-drerxvdvek-477[tvzgs]
otzkxtgzoutgr-hatte-jkbkruvsktz-748[yutkm]
ksodcbwnsr-qcbgiasf-ufors-pibbm-rsdzcmasbh-298[sbcra]
dmbttjgjfe-qmbtujd-hsbtt-bobmztjt-259[mkyef]
lnkfaypeha-bhksan-wymqeoepekj-836[lcygv]
zekvierkzferc-treup-ljvi-kvjkzex-789[ekrvz]
ajyqqgdgcb-djmucp-mncpyrgmlq-626[cyuom]
sbnqbhjoh-fhh-bdrvjtjujpo-857[bmhse]
surmhfwloh-iorzhu-vklsslqj-829[hlsor]
ymszqfuo-nmewqf-iadwetab-690[unsbc]
gpewwmjmih-tpewxmg-kveww-xvemrmrk-464[mrtux]
rzvkjiduzy-nxvqzibzm-cpio-mzxzdqdib-395[lnkyz]
qzoggwtwsr-suu-kcfygvcd-766[gcsuw]
molgbzqfib-bdd-rpbo-qbpqfkd-679[tljei]
gcfcnuls-aluxy-vcibutulxiom-vohhs-uhufsmcm-110[mstvf]
nzcczdtgp-clmmte-lylwjdtd-561[puhls]
hqcfqwydw-fbqijys-whqii-ijehqwu-166[czvwd]
ytu-xjhwjy-wfintfhynaj-uqfxynh-lwfxx-xjwanhjx-567[syfzw]
ujoon-ytaanqtpc-itrwcdadvn-895[ntmsp]
xzwrmkbqtm-xtiabqk-oziaa-zmamizkp-460[amzik]
rwcnawjcrxwju-snuuhknjw-jlzdrbrcrxw-979[rwjcn]
oknkvcta-itcfg-ecpfa-octmgvkpi-414[cktaf]
kdijqrbu-uww-mehaixef-348[oyzxu]
ncjzrpytn-hplazytkpo-prr-hzcvdsza-249[yvxgz]
qczcftiz-pibbm-hfowbwbu-870[bcfiw]
xqvwdeoh-fdqgb-dftxlvlwlrq-777[ymaiz]
rgllk-qss-ruzmzouzs-482[ynsqw]
eadalsjq-yjsvw-jsttal-ksdwk-112[mlgwj]
sbqiiyvyut-isqludwuh-xkdj-efuhqjyedi-166[iudqy]
ziuxioqvo-kpwkwtibm-xczkpiaqvo-382[jucqm]
jef-iushuj-sqdto-seqjydw-skijecuh-iuhlysu-322[sbnmo]
hqcfqwydw-uww-sedjqydcudj-816[krxlq]
shmml-qlr-znexrgvat-741[twjzq]
elrkdcdugrxv-gbh-pdunhwlqj-153[sunto]
nsyjwsfyntsfq-gfxpjy-hzxytrjw-xjwanhj-385[jyfns]
irdgrxzex-sleep-jkfirxv-867[ikstj]
mybbycsfo-mkxni-dbksxsxq-666[nmotl]
xmtjbzidx-xcjxjgvoz-mznzvmxc-525[acpvh]
zilqwikbqdm-ntwemz-zmikycqaqbqwv-642[cxfge]
pkl-oaynap-xwogap-iwjwcaiajp-290[cedyr]
zlilocri-ciltbo-zrpqljbo-pbosfzb-757[bloiz]
foadouwbu-suu-aobousasbh-896[uoabs]
lzfmdshb-okzrshb-fqzrr-zbpthrhshnm-859[poznx]
wifilzof-mwupyhayl-bohn-nywbhifias-994[neotf]
pbybeshy-rtt-ynobengbel-845[beynt]
ohmnuvfy-mwupyhayl-bohn-guleyncha-188[sdqab]
mvhkvbdib-wpiit-mzvxlpdndodji-811[uxmls]
jxdkbqfz-oxyyfq-pqloxdb-991[qxbdf]
sxdobxkdsyxkv-lexxi-nocsqx-640[jlfha]
shoewudys-sqdto-jhqydydw-478[dsyho]
xtwtelcj-rclop-upwwjmply-epnsyzwzrj-821[kdcvu]
sehheiylu-vbemuh-qsgkyiyjyed-192[rmqpn]
fmsledevhsyw-fyrrc-wxsveki-516[bzgvw]
jfifqxov-doxab-pzxsbkdbo-erkq-jxkxdbjbkq-939[bxkdj]
bnknqetk-cxd-bnmszhmldms-547[jcdas]
jsehsyafy-vqw-dgyaklauk-996[ayksd]
rdadguja-qjccn-uxcpcrxcv-921[gyvhm]
lxuxaodu-mhn-bnaerlnb-693[nablu]
ymszqfuo-otaoaxmfq-pqhqxabyqzf-794[kvfeg]
ykhknbqh-bhksan-hwxknwpknu-238[hswtq]
veqtekmrk-tpewxmg-kveww-hitpscqirx-646[mpoxs]
zhdsrqlchg-pdjqhwlf-edvnhw-vwrudjh-491[hdwjl]
tcrjjzwzvu-upv-jvimztvj-867[xbyim]
qzchnzbshud-qzaahs-dmfhmddqhmf-261[gxmsf]
vxupkizork-xghhoz-zkinturume-488[brhyz]
raphhxuxts-hrpktcvtg-wjci-sthxvc-765[htcxp]
ujqgywfau-wyy-mkwj-lwklafy-164[wyafj]
ubhatstkwhnl-unggr-wxiehrfxgm-553[yqtez]
gifavtkzcv-vxx-jkfirxv-971[vxfik]
xgjougizobk-hatte-xkgiwaoyozout-150[vsazb]
nij-mywlyn-mwupyhayl-bohn-womnigyl-mylpcwy-734[ysutv]
kwtwznct-kivlg-kwibqvo-tijwzibwzg-850[wiktz]
nij-mywlyn-wuhxs-wiuncha-yhachyylcha-266[aznkv]
pkl-oaynap-bhksan-nayaerejc-602[phqso]
oxjmxdfkd-zxkav-zlxqfkd-lmboxqflkp-419[xkdfl]
jshzzpmplk-zjhclunly-obua-zopwwpun-617[vzouh]
xgvnndadzy-ezggtwzvi-xpnojhzm-nzmqdxz-499[zndgx]
glrcplyrgmlyj-aylbw-amyrgle-amlryglkclr-938[abmon]
xcitgcpixdcpa-hrpktcvtg-wjci-igpxcxcv-219[cipxg]
muqfedyput-isqludwuh-xkdj-udwyduuhydw-868[udwyh]
fkqbokxqflkxi-yflexwxoalrp-pzxsbkdbo-erkq-absbilmjbkq-159[bkxlq]
tmrszakd-cxd-zbpthrhshnm-781[hdmrs]
kpvgtpcvkqpcn-ejqeqncvg-wugt-vguvkpi-284[efhns]
xqvwdeoh-mhoobehdq-frqwdlqphqw-933[jzuyw]
pynffvsvrq-wryylorna-bcrengvbaf-689[rfnvy]
qmpmxevc-kvehi-fyrrc-wepiw-932[entmr]
qzlozfhmf-bzmcx-bnzshmf-knfhrshbr-755[fhzbm]
awzwhofm-ufors-rms-obozmgwg-610[omwfg]
emixwvqhml-kpwkwtibm-lmxizbumvb-460[nkcey]
zgmfyxypbmsq-hcjjwzcyl-asqrmkcp-qcptgac-652[fnjvm]
yaxsnlcrun-ljwmh-mnyjacvnwc-901[vbxwn]
buzahisl-jhukf-jvhapun-thyrlapun-435[gcdyo]
jsvagsulanw-hdsklau-yjskk-kzahhafy-476[qkyzs]
rzvkjiduzy-agjrzm-yzqzgjkhzio-135[zjgik]
udglrdfwlyh-edvnhw-zrunvkrs-205[drhln]
mrxivrexmsrep-jpsaiv-pefsvexsvc-698[esvpr]
xzwrmkbqtm-kzgwomvqk-zijjqb-nqvivkqvo-642[cabgs]
rzvkjiduzy-zbb-nvgzn-551[zbnvd]
ncjzrpytn-nsznzwlep-ecltytyr-327[ntyzc]
raphhxuxts-gpqqxi-bpcpvtbtci-115[nzslk]
fmsledevhsyw-gerhc-wxsveki-100[stmxw]
rgndvtcxr-xcitgcpixdcpa-uadltg-rdcipxcbtci-531[cditx]
rdadguja-snt-igpxcxcv-895[acdgx]
ide-htrgti-rdggdhxkt-ytaanqtpc-htgkxrth-921[tcpfv]
sawlkjevaz-ywjzu-klanwpekjo-758[ajkwe]
hjgbwuladw-jsvagsulanw-hdsklau-yjskk-kwjnauwk-996[ucavp]
wfummczcyx-dyffsvyuh-xyjulngyhn-188[xnufp]
yuxufmdk-sdmpq-omzpk-pqbxakyqzf-690[pstoj]
wfummczcyx-willimcpy-vumeyn-yhachyylcha-708[piodu]
sxdobxkdsyxkv-cmkfoxqob-rexd-nozkbdwoxd-614[nmdwp]
dmbttjgjfe-gmpxfs-vtfs-uftujoh-961[ftjgm]
lnkfaypeha-zua-skngodkl-732[zyntx]
hqtyeqsjylu-uww-kiuh-juijydw-530[ujwyh]
mbiyqoxsm-zvkcdsm-qbkcc-yzobkdsyxc-146[onlmp]
wlqqp-upv-ivtvzmzex-165[fmczd]
cjpibabsepvt-fhh-dvtupnfs-tfswjdf-389[bzdyv]
kzgwomvqk-jcvvg-bmkpvwtwog-252[zelhm]
htsxzrjw-lwfij-hfsid-htfynsl-ywfnsnsl-567[ivjzs]
ide-htrgti-qphzti-gtprfjxhxixdc-401[fcapt]
qvbmzvibqwvit-uiovmbqk-xtiabqk-oziaa-lmxizbumvb-564[rotyq]
diozmivodjivg-ytz-yzkvmohzio-109[omrxn]
njmjubsz-hsbef-qmbtujd-hsbtt-eftjho-701[bjths]
krxqjijamxdb-snuuhknjw-anbnjalq-433[tkemh]
avw-zljyla-yhiipa-ylzlhyjo-149[zphyt]
nzydfxpc-rclop-clmmte-pyrtyppctyr-899[mjzsr]
bqxnfdmhb-oqnidbshkd-rbzudmfdq-gtms-knfhrshbr-365[bdhfm]
nvrgfezqvu-treup-tfrkzex-rercpjzj-347[rezfj]
gcfcnuls-aluxy-wuhxs-wiuncha-fiacmncwm-526[cuanw]
amjmpdsj-djmucp-kypicrgle-964[ftznh]
hvbizodx-wvnfzo-mzxzdqdib-655[dzcnu]
tagzsrsjvgmk-jsvagsulanw-vqw-vwhsjlewfl-892[tjlop]
mvkccspson-mrymyvkdo-bomosfsxq-952[mosck]
lqwhuqdwlrqdo-fkrfrodwh-frqwdlqphqw-153[jnwkm]
surmhfwloh-mhoobehdq-uhdftxlvlwlrq-153[nyvqs]
dlhwvupglk-ibuuf-klclsvwtlua-565[doeyn]
pwcvonofrcig-gqojsbusf-vibh-fsoqeiwgwhwcb-376[jcdlh]
muqfedyput-hqrryj-efuhqjyedi-998[equyd]
hwbba-hnqygt-fgrctvogpv-466[slvyu]
wfummczcyx-wbiwifuny-xyjulngyhn-916[spycn]
zilqwikbqdm-xtiabqk-oziaa-mvoqvmmzqvo-304[rxhzs]
xzwrmkbqtm-ntwemz-nqvivkqvo-954[gztdk]
dyz-combod-lkcuod-bomosfsxq-198[zyvju]
pbafhzre-tenqr-enoovg-phfgbzre-freivpr-455[cakfs]
tfcfiwlc-avccpsvre-jkfirxv-217[obgiy]
udpsdjlqj-gbh-vdohv-257[fpnes]
bwx-amkzmb-moo-zmkmqdqvo-330[whxfs]
raphhxuxts-tvv-jhtg-ithixcv-401[fyiab]
sorozgxe-mxgjk-laffe-vrgyzoi-mxgyy-xkykgxin-878[vkjnu]
clotzlnetgp-mldvpe-epnsyzwzrj-613[qdmpu]
gokzyxsjon-zvkcdsm-qbkcc-domrxyvyqi-224[ckoyd]
mtzslklcozfd-ojp-hzcvdsza-795[hvasg]
pxtihgbsxw-cxeeruxtg-labiibgz-475[ztyng]
mtzslklcozfd-nsznzwlep-cplnbftdtetzy-353[zuofx]
emixwvqhml-moo-zmamizkp-538[hvrjm]
foadouwbu-pibbm-oqeiwgwhwcb-168[mfiwn]
qyujihctyx-mwupyhayl-bohn-jolwbumcha-240[hyuab]
sxdobxkdsyxkv-pejji-mkxni-ckvoc-926[bktwh]
nglmtuex-ietlmbv-zktll-etuhktmhkr-345[tlekm]
qekrixmg-tpewxmg-kveww-wepiw-724[wegik]
oaddaeuhq-dmnnuf-fdmuzuzs-326[ersqt]
ktwbhtvmbox-xzz-vnlmhfxk-lxkobvx-943[yzabx]
zvyvgnel-tenqr-enoovg-npdhvfvgvba-117[cadbz]
vhehkyne-vtgwr-lmhktzx-579[hektv]
kzgwomvqk-zijjqb-bmkpvwtwog-148[njtma]
fubrjhqlf-fdqgb-zrunvkrs-907[ormsl]
oqnidbshkd-rbzudmfdq-gtms-kzanqzsnqx-859[suagv]
upq-tfdsfu-dboez-mbcpsbupsz-779[srtpm]
ugjjgkanw-hdsklau-yjskk-lwuzfgdgyq-632[gkjua]
oxmeeuruqp-ngzzk-fqotzaxask-326[aymzt]
eqnqthwn-dcumgv-ugtxkegu-596[nfath]
ygcrqpkbgf-uecxgpigt-jwpv-eqpvckpogpv-648[qsxvr]
udglrdfwlyh-hjj-zrunvkrs-829[csnzf]
vhkkhlbox-vtgwr-vhtmbgz-ftgtzxfxgm-657[sojpi]
luxciuwncpy-vcibutulxiom-vumeyn-ijyluncihm-708[dtmyw]
xst-wigvix-ikk-qevoixmrk-646[wuqfg]
ide-htrgti-gpqqxi-gtrtxkxcv-947[lzybn]
udglrdfwlyh-fdqgb-frdwlqj-vwrudjh-179[oqkrh]
ipvohghykvbz-kfl-klzpnu-617[khlpv]
oxaflxzqfsb-yxphbq-pxibp-653[afqdk]
bkzrrhehdc-idkkxadzm-cdudknoldms-105[dkchm]
zsxyfgqj-gfxpjy-hzxytrjw-xjwanhj-723[zstyw]
kfg-jvtivk-treup-tfrkzex-ivrthlzjzkzfe-997[ktzef]
zekvierkzferc-treup-tfrkzex-uvgcfpdvek-971[ekwcg]
xgsvgmotm-igtje-iugzotm-xkykgxin-358[mzwst]
jyfvnlupj-ihzrla-yljlpcpun-539[ljpnu]
bkwzkqsxq-zbytomdsvo-lkcuod-domrxyvyqi-692[odkqy]
pyknyegle-cee-qfgnngle-756[muevb]
buzahisl-zjhclunly-obua-yljlpcpun-461[cfmdj]
oxjmxdfkd-gbiivybxk-absbilmjbkq-731[uhjdc]
uqtqbizg-ozilm-kzgwomvqk-jcvvg-ikycqaqbqwv-798[qvgik]
ohmnuvfy-wbiwifuny-nluchcha-786[hnucf]
sbnqbhjoh-dboez-bdrvjtjujpo-753[dpmzu]
jyddc-glsgspexi-pskmwxmgw-100[aeylk]
qvbmzvibqwvit-xzwrmkbqtm-jiasmb-ikycqaqbqwv-902[qbimv]
htqtwkzq-idj-zxjw-yjxynsl-983[zvyre]
xekdwvwnzkqo-ejpanjwpekjwh-ywjzu-oanreyao-914[wejak]
sedikcuh-whqtu-sbqiiyvyut-isqludwuh-xkdj-skijecuh-iuhlysu-322[sktui]
rkpqxyib-bdd-xkxivpfp-471[pxbdi]
qxdwpopgsdjh-rpcsn-rdpixcv-jhtg-ithixcv-895[pcdhi]
mbggf-yhiipa-klclsvwtlua-955[oelkb]
eadalsjq-yjsvw-hjgbwuladw-bwddqtwsf-jwsuimakalagf-372[rpxet]
hmsdqmzshnmzk-rbzudmfdq-gtms-cdoknxldms-859[ywtqf]
bnqqnrhud-bzmcx-bnzshmf-qdbdhuhmf-625[smnwl]
vagreangvbany-onfxrg-qrcyblzrag-195[szmkx]
nij-mywlyn-wuhxs-mufym-916[sbczy]
xst-wigvix-hci-asvowlst-958[istvw]
lnkfaypeha-lhwopey-cnwoo-paydjkhkcu-680[lstyr]
veqtekmrk-fewoix-gywxsqiv-wivzmgi-646[kvuxl]
jvyyvzpcl-wshzapj-nyhzz-klzpnu-929[zpyhj]
amlqskcp-epybc-djmucp-sqcp-rcqrgle-730[opija]
sbqiiyvyut-isqludwuh-xkdj-cqhaujydw-998[yqrzk]
kwzzwaqdm-rmttgjmiv-xczkpiaqvo-928[smyzo]
zekvierkzferc-lejkrscv-gcrjkzt-xirjj-uvjzxe-321[svyma]
pbybeshy-rtt-fuvccvat-949[izmnw]
oxaflxzqfsb-zxkav-ixyloxqlov-133[mplun]
apwmeclga-aylbw-amyrgle-pcqcypaf-600[bimqc]
iqmbazulqp-nmewqf-mzmxkeue-144[oveiw]
udglrdfwlyh-edvnhw-hqjlqhhulqj-985[cpsor]
pinovwgz-zbb-gvwjmvojmt-655[dvsby]
qfmcusbwq-rms-kcfygvcd-688[cfmqs]
tbxmlkfwba-zxkav-zlxqfkd-jxohbqfkd-523[ljhnt]
gsrwyqiv-kvehi-gsvvswmzi-wgezirkiv-lyrx-hitevxqirx-100[yfbno]
etyyx-qzaahs-bnmszhmldms-599[msahy]
mvhkvbdib-nxvqzibzm-cpio-mzvxlpdndodji-473[rtjeu]
wlsiayhcw-vumeyn-ijyluncihm-994[yziwj]
oaddaeuhq-dmpuamofuhq-qss-fqotzaxask-898[ycmns]
ynukcajey-ywjzu-zalwnpiajp-108[vmosc]
dzczkrip-xiruv-tyftfcrkv-uvgrikdvek-529[bdmtn]
clxalrtyr-nlyoj-xlcvpetyr-379[wexcp]
zlkprjbo-doxab-bdd-ixyloxqlov-419[nitur]
uiovmbqk-rmttgjmiv-bmkpvwtwog-850[lsyvi]
dfcxsqhwzs-pibbm-aofyshwbu-168[mtsnf]
lhkhszqx-fqzcd-eknvdq-cdrhfm-287[dhqcf]
cvabijtm-lgm-ivitgaqa-694[rpzkl]
qzlozfhmf-rbzudmfdq-gtms-zbpthrhshnm-963[hmzfb]
bxaxipgn-vgpst-qjccn-detgpixdch-921[cgpxd]
krxqjijamxdb-kjbtnc-cajrwrwp-771[liezd]
surmhfwloh-vfdyhqjhu-kxqw-rshudwlrqv-387[bzfdx]
dlhwvupglk-ihzrla-dvyrzovw-643[lvdhr]
dlhwvupglk-lnn-zopwwpun-435[lnpwu]
sbnqbhjoh-sbccju-ufdiopmphz-519[bhcjo]
oaxadrgx-otaoaxmfq-etubbuzs-820[aoxbt]
encuukhkgf-lgnnadgcp-nqikuvkeu-648[jhcwv]
ajyqqgdgcb-zyqicr-bcqgel-964[zyesc]
kmjezxodgz-wvnfzo-xpnojhzm-nzmqdxz-681[wrjtn]
fnjyxwrinm-kjbtnc-mnyjacvnwc-277[mjtln]
ktfitzbgz-cxeeruxtg-nlxk-mxlmbgz-527[yiwvu]
tbxmlkfwba-avb-pqloxdb-887[balxd]
pbybeshy-fpniratre-uhag-ynobengbel-689[nqied]
emixwvqhml-lgm-aitma-174[maile]
ryexqpqhteki-rqiauj-husuylydw-686[pmutv]
njmjubsz-hsbef-tdbwfohfs-ivou-fohjoffsjoh-337[fohjs]
lnkfaypeha-ydkykhwpa-nayaerejc-394[mwhrf]
pybgmyargtc-zsllw-qyjcq-964[ctgad]
myvybpev-cmkfoxqob-rexd-ckvoc-198[ueqjn]
votubcmf-ezf-sftfbsdi-285[nvymk]
hwdtljsnh-gzssd-jslnsjjwnsl-671[pimqy]
votubcmf-dipdpmbuf-mbcpsbupsz-441[lckdr]
ide-htrgti-gpqqxi-rjhidbtg-htgkxrt-193[gynxm]
yhwooebeaz-ydkykhwpa-opknwca-290[yqzkj]
nbhofujd-tdbwfohfs-ivou-tbmft-493[tjgzf]
xgsvgmotm-kmm-rumoyzoiy-358[vzysu]
etaqigpke-fag-fgukip-154[gaefi]
sbnqbhjoh-sbccju-tfswjdft-961[bjscf]
hvbizodx-kgvnodx-bmvnn-adivixdib-629[pabrd]
xfbqpojafe-qmbtujd-hsbtt-usbjojoh-103[bjotf]
ohmnuvfy-wbiwifuny-wihnuchgyhn-422[fdwyt]
wifilzof-vohhs-lymyulwb-448[iuvhx]
owshgfarwv-hdsklau-yjskk-ogjckzgh-606[kghsa]
sorozgxe-mxgjk-yigbktmkx-natz-zxgototm-800[gotxk]
lejkrscv-tyftfcrkv-jvimztvj-399[tjimr]
gsvvswmzi-nippcfier-wivzmgiw-932[zybmh]
odiih-ljwmh-lxjcrwp-uxprbcrlb-979[lrbch]
uzfqdzmfuazmx-vqxxknqmz-ruzmzouzs-404[oglmz]
kyelcrga-bwc-qyjcq-366[mzens]
foadouwbu-gqojsbusf-vibh-gsfjwqsg-688[yfqzi]
kfg-jvtivk-gcrjkzt-xirjj-ivtvzmzex-581[jvikt]
ckgvutofkj-igtje-giwaoyozout-332[cwijt]
pbybeshy-pubpbyngr-erfrnepu-923[bpery]
hcd-gsqfsh-dzoghwq-ufogg-gozsg-532[mqopr]
wfummczcyx-wuhxs-wiuncha-yhachyylcha-188[hxcrd]
ujqgywfau-tmffq-ljsafafy-112[bfytz]
clxalrtyr-ojp-qtylyntyr-119[gijln]
lmprfnmjc-mzhcar-qrmpyec-548[mcrpa]
yhwooebeaz-oywrajcan-dqjp-ajcejaanejc-316[gbruk]
wifilzof-xsy-yhachyylcha-604[hstyz]
ziuxioqvo-ntwemz-tijwzibwzg-460[qjaft]
qspkfdujmf-kfmmzcfbo-gjobodjoh-103[qcemb]
sbqiiyvyut-tou-jusxdebewo-764[rwmyx]
surmhfwloh-edvnhw-pdunhwlqj-699[retcb]
mvkccspson-zvkcdsm-qbkcc-ecob-docdsxq-198[csdko]
pbybeshy-wryylorna-pbagnvazrag-429[vnjmx]
vdzonmhydc-bzmcx-trdq-sdrshmf-937[kigbu]
qzoggwtwsr-pibbm-rsdzcmasbh-454[lnqsc]
fodvvlilhg-gbh-dqdobvlv-153[vdlbg]
iuruxlar-xgjougizobk-igtje-vaxingyotm-696[gioux]
rmn-qcapcr-qaytclecp-fslr-qrmpyec-314[cztqy]
nvrgfezqvu-srjbvk-crsfirkfip-373[rfvik]
xtwtelcj-rclop-tyepcyletzylw-qwzhpc-opgpwzaxpye-717[mdzsw]
sxdobxkdsyxkv-lexxi-dbksxsxq-744[wzmfo]
bnqqnrhud-cxd-otqbgzrhmf-911[zqmyx]
kmjezxodgz-xjinphzm-bmvyz-ytz-gvwjmvojmt-343[mzjvg]
hplazytkpo-mldvpe-pyrtyppctyr-951[pgoxs]
dzczkrip-xiruv-treup-ljvi-kvjkzex-867[newix]
gsrwyqiv-kvehi-gerhc-vieguymwmxmsr-516[egimr]
rgllk-otaoaxmfq-ymdwqfuzs-924[aflmo]
pualyuhapvuhs-kfl-wbyjohzpun-461[uhpal]
vagreangvbany-cebwrpgvyr-pnaql-erfrnepu-481[hmnwj]
wsvsdkbi-qbkno-oqq-domrxyvyqi-354[xyfjg]
ykjoqian-cnwza-xwogap-odellejc-992[utznj]
bkwzkqsxq-oqq-ecob-docdsxq-718[vhbka]
yaxsnlcrun-kjbtnc-fxatbqxy-745[ysrtb]
uwtojhynqj-rflsjynh-uqfxynh-lwfxx-tujwfyntsx-307[vulsb]
dmybmsuzs-otaoaxmfq-eqdhuoqe-950[zhwyv]
gokzyxsjon-tovvilokx-nocsqx-978[oxkns]
oazegyqd-sdmpq-rgllk-otaoaxmfq-pqeusz-976[qaode]
pejji-bkllsd-vyqscdsmc-614[scdjl]
nwzekwypera-ywjzu-zarahkliajp-758[bahgf]
zuv-ykixkz-laffe-yigbktmkx-natz-jkvruesktz-774[trdse]
pelbtravp-cynfgvp-tenff-npdhvfvgvba-845[lgrst]
zlkprjbo-doxab-avb-obpbxoze-549[cobza]
ujqgywfau-aflwjfslagfsd-bwddqtwsf-ljsafafy-424[wcozk]
rdchjbtg-vgpst-hrpktcvtg-wjci-gthtpgrw-193[jsqvi]
ixeumktoi-vrgyzoi-mxgyy-ygrky-514[grzvh]
wkqxodsm-nio-bomosfsxq-588[osmqx]
pbybeshy-onfxrg-fgbentr-715[ahftx]
pdjqhwlf-fdqgb-dftxlvlwlrq-829[lbrgj]
ejpanjwpekjwh-nwxxep-ykjpwejiajp-602[mtcnj]
npmhcargjc-cee-rcaflmjmew-860[cemaj]
zuv-ykixkz-lruckx-ygrky-748[kyrux]
myvybpev-lexxi-bomosfsxq-822[xbemo]
ipvohghykvbz-qlssfilhu-aljouvsvnf-591[frsvt]
hqtyeqsjylu-fbqijys-whqii-huqsgkyiyjyed-660[stpzn]
irgyyolokj-inuiurgzk-rghuxgzuxe-124[guirk]
xmrrq-ugjjgkanw-wyy-umklgewj-kwjnauw-736[wjgku]
fydelmwp-clmmte-xlylrpxpye-847[lempy]
tfiifjzmv-avccpsvre-dribvkzex-685[tvxrq]
iqmbazulqp-qss-pqbxakyqzf-508[yxnth]
iuruxlar-houngfgxjuay-igtje-iugzotm-ktmotkkxotm-618[dtvzi]
lhkhszqx-fqzcd-bzmcx-rsnqzfd-495[wtxeb]
sebehvkb-sqdto-cqdqwucudj-348[dqbce]
hdgdovmt-bmvyz-agjrzm-xpnojhzm-nzmqdxz-343[tsxdr]
tfcfiwlc-irsszk-wzeretzex-477[thmsr]
awzwhofm-ufors-qobrm-cdsfohwcbg-168[tofxm]
gpewwmjmih-hci-eguymwmxmsr-958[mjnya]
clxalrtyr-clotzlnetgp-awldetn-rcldd-opdtry-171[hynzs]
rgllk-otaoaxmfq-ruzmzouzs-118[ozalm]
zgmfyxypbmsq-djmucp-qyjcq-574[hbayt]
shoewudys-sqdto-seqjydw-tuiywd-608[kdalb]
gokzyxsjon-mkxni-vyqscdsmc-432[sckmn]
enzcntvat-pnaql-grpuabybtl-585[antbl]
sehheiylu-isqludwuh-xkdj-jusxdebewo-400[pjhum]
kmjezxodgz-ezggtwzvi-jkzmvodjin-369[zgjde]
xcitgcpixdcpa-qjccn-detgpixdch-739[aohtz]
ksodcbwnsr-tzcksf-fsqswjwbu-714[swbcf]
lxaaxbren-kjbtnc-jlzdrbrcrxw-225[nwkot]
mvydjvxodqz-nxvqzibzm-cpio-hvmfzodib-733[vzdim]
sbejpbdujwf-dboez-dvtupnfs-tfswjdf-363[youlh]
mtzslklcozfd-mldvpe-cpdplcns-275[lzyck]
nvrgfezqvu-sleep-kirzezex-607[bwxna]
qekrixmg-gerhc-xiglrspskc-204[dcozr]
ktwbhtvmbox-vtgwr-vhtmbgz-wxiehrfxgm-449[tbghm]
etaqigpke-ecpfa-tgegkxkpi-674[bopve]
kwtwznct-jiasmb-ikycqaqbqwv-252[qwabc]
oxaflxzqfsb-yxphbq-ildfpqfzp-939[endsq]
qcffcgwjs-suu-gvwddwbu-272[byfto]
lhkhszqx-fqzcd-cxd-lzmzfdldms-391[dzlcf]
iutyaskx-mxgjk-lruckx-uvkxgzouty-254[uvfmo]
nzwzcqfw-mldvpe-zapcletzyd-483[aznms]
luxciuwncpy-wbiwifuny-ijyluncihm-396[xuqsy]
rgndvtcxr-hrpktcvtg-wjci-sthxvc-401[krmqs]
tyepcyletzylw-nlyoj-nzletyr-ecltytyr-457[zrxqh]
zsxyfgqj-rnqnyfwd-lwfij-kqtbjw-uzwhmfxnsl-307[fwjnq]
qjopwxha-oywrajcan-dqjp-oanreyao-862[zwomt]
pwcvonofrcig-dzoghwq-ufogg-hfowbwbu-844[ogwfb]
bgmxkgtmbhgte-ietlmbv-zktll-vhgmtbgfxgm-787[gmtbl]
eza-dpncpe-mldvpe-cpdplcns-405[uobym]
qmpmxevc-kvehi-gerhc-gsexmrk-gywxsqiv-wivzmgi-464[dafcm]
joufsobujpobm-dpssptjwf-kfmmzcfbo-tbmft-961[fbmoj]
foadouwbu-xszzmpsob-rsgwub-324[ubxcr]
ucynmlgxcb-aylbw-qfgnngle-210[tfzcn]
sorozgxe-mxgjk-lruckx-uvkxgzouty-254[mnvbw]
vxupkizork-kmm-jkyomt-384[kmoij]
mhi-lxvkxm-vtgwr-phkdlahi-761[hiklm]
ixccb-fkrfrodwh-ghyhorsphqw-335[hrcfo]
hcd-gsqfsh-dzoghwq-ufogg-rsjszcdasbh-168[rfxyw]
tbxmlkfwba-zxkav-pbosfzbp-965[ipmzy]
xcitgcpixdcpa-qxdwpopgsdjh-uadltg-uxcpcrxcv-167[bjvrp]
etyyx-cxd-kzanqzsnqx-573[bmaui]
cybyjqho-whqtu-hqrryj-efuhqjyedi-530[czdbf]
votubcmf-kfmmzcfbo-efqbsunfou-597[fbmou]
awzwhofm-ufors-tzcksf-sbuwbssfwbu-272[rsubo]
kwzzwaqdm-kivlg-kwibqvo-amzdqkma-356[inmyj]
ixccb-fdqgb-zrunvkrs-569[etxgi]
rdchjbtg-vgpst-egdytrixat-qjccn-rdcipxcbtci-713[duwnc]
mbiyqoxsm-tovvilokx-psxkxmsxq-978[xmosi]
xgvnndadzy-xviyt-rjmfncjk-707[josem]
aczupnetwp-awldetn-rcldd-nfdezxpc-dpcgtnp-873[svdjf]
ahngzyzqcntr-bzmcx-sdbgmnknfx-859[nzbcg]
sorozgxe-mxgjk-igtje-jkvgxzsktz-696[gjkxz]
rgllk-dmybmsuzs-omzpk-oamfuzs-pqhqxabyqzf-456[alknr]
aflwjfslagfsd-xdgowj-hmjuzskafy-528[fajsd]
htwwtxnaj-hmthtqfyj-htsyfnsrjsy-879[hnldm]
gokzyxsjon-lexxi-nozvyiwoxd-640[fziuy]
pbeebfvir-cynfgvp-tenff-genvavat-819[efvna]
pybgmyargtc-djmucp-bcqgel-184[rfmta]
myvybpev-mbiyqoxsm-oqq-dbksxsxq-926[tbqzr]
xmtjbzidx-xviyt-yzqzgjkhzio-499[sptmq]
iruzfrtkzmv-irsszk-ivtvzmzex-659[zirvk]
xst-wigvix-nippcfier-erepcwmw-126[iepwc]
amlqskcp-epybc-aylbw-amyrgle-pcacgtgle-730[mpskn]
pybgmyargtc-qaytclecp-fslr-jyzmpyrmpw-756[syuvq]
kwzzwaqdm-kivlg-kwibqvo-tijwzibwzg-746[zrpnw]
frqvxphu-judgh-hjj-vklsslqj-543[myczb]
tcorcikpi-tcfkqcevkxg-rncuvke-itcuu-rwtejcukpi-154[jyoui]
mybbycsfo-excdklvo-zvkcdsm-qbkcc-nocsqx-744[rpzts]
emixwvqhml-xtiabqk-oziaa-nqvivkqvo-850[voxnr]
wlqqp-avccpsvre-jrcvj-945[cvjpq]
rgndvtcxr-qphzti-itrwcdadvn-713[drtci]
zhdsrqlchg-sodvwlf-judvv-fxvwrphu-vhuylfh-335[sgotp]
jchipqat-uadltg-tcvxcttgxcv-219[lquds]
gntmfefwitzx-hfsid-rfwpjynsl-931[ubayg]
apwmeclga-njyqrga-epyqq-nspafyqgle-964[aqegp]
xgjougizobk-hatte-xkykgxin-592[hczyv]
zgmfyxypbmsq-afmamjyrc-nspafyqgle-106[nltfa]
jshzzpmplk-ihzrla-jbzavtly-zlycpjl-721[kzovn]
apwmeclga-hcjjwzcyl-rpyglgle-496[lvmqk]
kwtwznct-akidmvomz-pcvb-mvoqvmmzqvo-746[hgszx]
surmhfwloh-exqqb-dftxlvlwlrq-621[lqfhr]
dfcxsqhwzs-rms-sbuwbssfwbu-844[qcrnm]
ytu-xjhwjy-wfggny-jslnsjjwnsl-541[jnswy]
zovldbkfz-gbiivybxk-obzbfsfkd-809[bfkzd]
lxwbdvna-pajmn-ajkkrc-anlnrerwp-147[amynk]
xjgjmapg-agjrzm-hvivbzhzio-811[tjpax]
willimcpy-xsy-lymyulwb-318[ytesn]
ckgvutofkj-lruckx-vaxingyotm-228[efntu]
zloolpfsb-avb-cfkxkzfkd-159[iyjts]
vhglnfxk-zktwx-ubhatstkwhnl-ietlmbv-zktll-kxvxbobgz-293[kltbx]
gokzyxsjon-mkxni-cdybkqo-952[tynps]
kfg-jvtivk-sleep-jyzggzex-373[egjkv]
hwdtljsnh-xhfajsljw-mzsy-wjxjfwhm-827[vuaex]
qxdwpopgsdjh-eaphixr-vgphh-jhtg-ithixcv-427[hpgix]
cebwrpgvyr-pubpbyngr-qrcyblzrag-299[ecfbk]
bwx-amkzmb-kivlg-lmxizbumvb-148[wjmyo]
bdavqofuxq-vqxxknqmz-fdmuzuzs-326[ezmtq]
laffe-hatte-ktmotkkxotm-410[ymcnz]
fkqbokxqflkxi-avb-zrpqljbo-pbosfzb-497[bfkoq]
ynssr-vetllbybxw-yehpxk-ftgtzxfxgm-241[xtybe]
dsxxw-djmucp-kypicrgle-444[gvxac]
dfcxsqhwzs-forwcoqhwjs-gqojsbusf-vibh-fsqswjwbu-220[ytwiz]
wfintfhynaj-xhfajsljw-mzsy-hzxytrjw-xjwanhj-307[tkzub]
ajvyjprwp-bljenwpna-qdwc-anbnjalq-459[sqrzn]
pyknyegle-dsxxw-bwc-kypicrgle-340[vgwsd]
dwbcjkun-mhn-ldbcxvna-bnaerln-485[vgsei]
wsvsdkbi-qbkno-pvygob-kxkvicsc-458[mynov]
qfkkj-upwwjmply-zapcletzyd-613[noqls]
bqxnfdmhb-qzaahs-zmzkxrhr-989[wzhlt]
apwmeclga-afmamjyrc-dglylagle-860[algmc]
jyfvnlupj-msvdly-klwhyatlua-175[lyaju]
wlsiayhcw-dyffsvyuh-fuvilunils-422[iuzke]
fydelmwp-awldetn-rcldd-xlylrpxpye-873[rdnsj]
bkzrrhehdc-azrjds-ehmzmbhmf-287[hmrzb]
mvkccspson-bkllsd-dbksxsxq-926[skbcd]
qfmcusbwq-foppwh-rsdzcmasbh-870[sbcfh]
vrurcjah-pajmn-npp-fxatbqxy-381[apjnr]
vjpwncrl-yaxsnlcrun-kdwwh-uxprbcrlb-485[gylan]
lgh-kwujwl-tskcwl-ugflsafewfl-788[lwfgk]
avw-zljyla-jyfvnlupj-qlssfilhu-aljouvsvnf-409[ljvaf]
lejkrscv-jtrmvexvi-ylek-fgvirkzfej-763[evjkr]
cxy-bnlanc-kdwwh-lxwcjrwvnwc-277[umehn]
eza-dpncpe-clmmte-cplnbftdtetzy-145[ysezq]
pinovwgz-kgvnodx-bmvnn-vxlpdndodji-603[encyh]
gifavtkzcv-sleep-cfxzjkztj-919[vdzmb]
lnkfaypeha-ywjzu-ykwpejc-zaoecj-212[huvex]
oqnidbshkd-cxd-qdrdzqbg-573[kvsnt]
hcd-gsqfsh-foppwh-hfowbwbu-402[cldzy]
qvbmzvibqwvit-xtiabqk-oziaa-apqxxqvo-590[wbigl]
myxcewob-qbkno-zvkcdsm-qbkcc-nofovyzwoxd-198[vqfcu]
vehmsegxmzi-hci-xiglrspskc-542[isceg]
xekdwvwnzkqo-bhksan-wymqeoepekj-602[eynfr]
ujqgywfau-xdgowj-wfyafwwjafy-866[wfajy]
lxaaxbren-ajkkrc-ldbcxvna-bnaerln-303[anblr]
dpotvnfs-hsbef-kfmmzcfbo-dvtupnfs-tfswjdf-259[ufyek]
zhdsrqlchg-sodvwlf-judvv-uhfhlylqj-959[zjoag]
rdggdhxkt-uadltg-hwxeexcv-557[xigef]
nwzekwypera-acc-klanwpekjo-368[aekwc]
ymszqfuo-otaoaxmfq-mocgueufuaz-248[xgnem]
yaxsnlcrun-ljwmh-jwjuhbrb-459[ebkum]
oxaflxzqfsb-yflexwxoalrp-mixpqfz-doxpp-pxibp-107[ilnsk]
oaddaeuhq-pkq-ymdwqfuzs-638[yuczs]
pynffvsvrq-pnaql-pbngvat-qrirybczrag-845[ranpq]
tpspahyf-nyhkl-wshzapj-nyhzz-mpuhujpun-721[hpnuy]
kyelcrga-hcjjwzcyl-qrmpyec-392[cyejl]
lqwhuqdwlrqdo-exqqb-ghsorbphqw-491[uopyz]
rdadguja-eaphixr-vgphh-pcpanhxh-141[qmfpg]
yhtwhnpun-qlssfilhu-svnpzapjz-149[hnpsl]
xtwtelcj-rclop-nlyoj-qtylyntyr-249[ltycj]
rgndvtcxr-rpcsn-rdpixcv-ejgrwphxcv-193[kulpr]
qmpmxevc-kvehi-ikk-pefsvexsvc-542[aitns]
otzkxtgzoutgr-lruckx-xkgiwaoyozout-150[mdfyq]
zotts-luvvcn-lyuwkocmcncih-942[gjymz]
vqr-ugetgv-dwppa-fgrnqaogpv-544[tromz]
cjpibabsepvt-sbccju-nbslfujoh-545[mwkqj]
aczupnetwp-clmmte-xlcvpetyr-223[pmoqy]
rdadguja-ytaanqtpc-prfjxhxixdc-245[adxcj]
ucynmlgxcb-aylbw-amyrgle-bctcjmnkclr-236[uazni]
shmml-pnaql-pbngvat-grpuabybtl-585[jivfg]
dzczkrip-xiruv-irsszk-glityrjzex-867[nzayl]
pbafhzre-tenqr-fpniratre-uhag-pbagnvazrag-377[twsqp]
tinnm-dzoghwq-ufogg-cdsfohwcbg-636[fpxjq]
ixeumktoi-igtje-iugzotm-rumoyzoiy-904[trlzu]
pelbtravp-pnaql-pbngvat-freivprf-949[parvb]
gsrwyqiv-kvehi-nippcfier-tyvglewmrk-386[fctsn]
hqcfqwydw-vbemuh-jusxdebewo-400[xzfmv]
cjpibabsepvt-sbccju-efqmpznfou-935[rzenu]
fnjyxwrinm-npp-cajrwrwp-979[nprwj]
vetllbybxw-unggr-ehzblmbvl-501[fvmoa]
ugfkmewj-yjsvw-wyy-ghwjslagfk-710[vmcub]
etyyx-qzaahs-btrsnldq-rdquhbd-183[gfzym]
kzgwomvqk-lgm-camz-bmabqvo-902[nfmek]
xcitgcpixdcpa-hrpktcvtg-wjci-ejgrwphxcv-869[cpgit]
pbybeshy-pnaql-pbngvat-ernpdhvfvgvba-351[bpvan]
jxdkbqfz-zxkav-zlxqfkd-xkxivpfp-991[npdis]
raphhxuxts-qphzti-hwxeexcv-167[hxept]
oknkvcta-itcfg-tcorcikpi-fag-tgceswkukvkqp-362[qvgoc]
amlqskcp-epybc-glrcplyrgmlyj-zsllw-cleglccpgle-158[atcbx]
apwmeclga-aylbw-amyrgle-bcnjmwkclr-912[tnskp]
xjmmjndqz-zbb-vxlpdndodji-369[wfyzh]
shoewudys-rkddo-cqhaujydw-842[dhosu]
zovldbkfz-ciltbo-qoxfkfkd-289[ykmgw]
willimcpy-jfumncw-alumm-omyl-nymncha-396[isnbe]
vjpwncrl-lqxlxujcn-jwjuhbrb-303[epojm]
gzefmnxq-omzpk-pqbxakyqzf-352[zpnyf]
ytu-xjhwjy-kqtbjw-hzxytrjw-xjwanhj-281[zxolt]
esyfwlau-vqw-dstgjslgjq-788[xwpyu]
kyelcrga-pyzzgr-qfgnngle-834[rcqns]
ovbunmneqbhf-pubpbyngr-znexrgvat-533[vdezh]
veqtekmrk-fewoix-gsrxemrqirx-100[erxik]
hjgbwuladw-hdsklau-yjskk-esjcwlafy-216[ajkls]
pinovwgz-kmjezxodgz-zbb-vivgtndn-993[svekp]
xlrypetn-awldetn-rcldd-fdpc-epdetyr-301[delpr]
ajmrxjlcren-ljwmh-jlzdrbrcrxw-719[juazc]
ymszqfuo-omzpk-oamfuzs-ruzmzouzs-456[caspz]
gspsvjyp-tpewxmg-kveww-wepiw-776[zglbt]
eqnqthwn-ecpfa-fgrnqaogpv-440[mnlrz]
rflsjynh-hfsid-htfynsl-rfwpjynsl-489[ghblf]
pkl-oaynap-bhksan-nawymqeoepekj-368[aeknp]
hwdtljsnh-wfggny-wjxjfwhm-229[whjfg]
lqwhuqdwlrqdo-exqqb-xvhu-whvwlqj-725[rhaqf]
jyddc-nippcfier-erepcwmw-178[sticn]
eadalsjq-yjsvw-wyy-mkwj-lwklafy-736[yzjgq]
xst-wigvix-veffmx-jmrergmrk-646[nuewy]
elrkdcdugrxv-gbh-frqwdlqphqw-179[zshyg]
rdadguja-hrpktcvtg-wjci-jhtg-ithixcv-765[tcghi]
lejkrscv-zekvierkzferc-irsszk-rercpjzj-399[rekzc]
kmjezxodgz-nxvqzibzm-cpio-pnzm-oznodib-837[zoimn]
clotzlnetgp-clmmte-hzcvdsza-457[yxtba]
aoubshwq-gqojsbusf-vibh-hfowbwbu-428[bhosu]
bdavqofuxq-dmnnuf-eqdhuoqe-144[conrz]
xfbqpojafe-qmbtujd-hsbtt-pqfsbujpot-259[snmtz]
dlhwvupglk-msvdly-svnpzapjz-539[lpvds]
clotzlnetgp-dnlgpyrpc-sfye-opalcexpye-171[ykocp]
pejji-pvygob-bokmaescsdsyx-406[wmqnk]
lejkrscv-avccpsvre-glityrjzex-789[cervj]
enqvbnpgvir-wryylorna-erfrnepu-403[sdygr]
hplazytkpo-mldvpe-opawzjxpye-977[qtzrk]
sbejpbdujwf-dboez-tupsbhf-493[bdefj]
gsrwyqiv-kvehi-yrwxefpi-fewoix-vigimzmrk-672[sytnz]
jef-iushuj-zubboruqd-iqbui-946[kcysl]
surmhfwloh-hjj-dftxlvlwlrq-595[lhfjr]
zloolpfsb-oxyyfq-abmxoqjbkq-835[ynzmp]
zotts-mwupyhayl-bohn-xymcah-786[iyhxu]
zhdsrqlchg-mhoobehdq-fxvwrphu-vhuylfh-257[cxogf]
ktwbhtvmbox-ktuubm-tvjnblbmbhg-943[flrzj]
qzoggwtwsr-dfcxsqhwzs-rms-rsgwub-662[tehuv]
tfejldvi-xiruv-szfyrqriuflj-upv-wzeretzex-555[rpocq]
qzoggwtwsr-foppwh-twbobqwbu-844[wbogp]
molgbzqfib-avb-qoxfkfkd-289[tkrcd]
hqfxxnknji-uqfxynh-lwfxx-fhvznxnynts-177[fdlkw]
sno-rdbqds-dff-cdrhfm-287[dfrsb]
dkqjcbctfqwu-gii-wugt-vguvkpi-674[sjkzr]
wihmogyl-aluxy-wuhxs-mbcjjcha-968[eufrc]
vetllbybxw-wrx-vnlmhfxk-lxkobvx-969[ylumi]
rkpqxyib-avb-absbilmjbkq-835[wnjuy]
iuruxlar-igtje-sgxqkzotm-930[girtu]
slqryzjc-kyelcrga-bwc-dglylagle-496[lcgya]
fodvvlilhg-exqqb-xvhu-whvwlqj-673[vhlqw]
yknnkoera-fahhuxawj-nawymqeoepekj-628[sfgvu]
fhezusjybu-sqdto-jusxdebewo-166[vmzhw]
dlhwvupglk-qlssfilhu-klclsvwtlua-591[lsuhk]
oaxadrgx-eomhqzsqd-tgzf-pqbxakyqzf-716[nrgqs]
jfifqxov-doxab-oxjmxdfkd-oxyyfq-cfkxkzfkd-887[gbrxt]
dzczkrip-xiruv-tyftfcrkv-jrcvj-841[zcxdu]
ajmrxjlcren-snuuhknjw-mnyuxhvnwc-537[rjiwk]
kgjgrypw-epybc-njyqrga-epyqq-cleglccpgle-548[ykprd]
qjopwxha-xwogap-opknwca-264[nrlsc]
ejpanjwpekjwh-xwogap-odellejc-550[hndsm]
ziuxioqvo-kivlg-kwibqvo-camz-bmabqvo-616[iovbq]
kfg-jvtivk-szfyrqriuflj-upv-ivjvrity-607[drsmt]
diozmivodjivg-ezggtwzvi-yzkvmohzio-421[nyzbw]
gvcskirmg-hci-erepcwmw-464[kbwmq]
nchhg-kivlg-kwibqvo-mvoqvmmzqvo-460[vmoqg]
aoubshwq-rms-cdsfohwcbg-714[atihz]
ajvyjprwp-lqxlxujcn-cajrwrwp-901[ghfuv]
oqnidbshkd-rbzudmfdq-gtms-rghoohmf-339[mrkzl]
muqfedyput-hqrryj-iuhlysui-504[uyhiq]
kgjgrypw-epybc-djmucp-pcacgtgle-990[cgpej]
vcibutulxiom-wbiwifuny-yhachyylcha-682[uigky]
cebwrpgvyr-sybjre-freivprf-611[rebfp]
wlsiayhcw-dyffsvyuh-lyuwkocmcncih-994[cyhwf]
ncjzrpytn-fydelmwp-nsznzwlep-opalcexpye-795[penly]
wkqxodsm-cmkfoxqob-rexd-domrxyvyqi-692[qbnjg]
fmsledevhsyw-veffmx-viwievgl-490[utkwb]
kwzzwaqdm-lgm-ewzsapwx-200[wzamd]
foadouwbu-dzoghwq-ufogg-igsf-hsghwbu-506[tcdak]
rgndvtcxr-rpcsn-itrwcdadvn-297[cnzup]
bxaxipgn-vgpst-qjccn-prfjxhxixdc-115[qbscm]
vehmsegxmzi-tpewxmg-kveww-eguymwmxmsr-152[tucsj]
nbhofujd-sbejpbdujwf-qmbtujd-hsbtt-nbslfujoh-337[satqk]
bnknqetk-dff-zbpthrhshnm-989[hpvak]
hplazytkpo-nsznzwlep-opawzjxpye-431[nmfdl]
xjgjmapg-xviyt-xjvodib-ncdkkdib-473[hgbua]
nglmtuex-vtgwr-ltexl-137[tjwsv]
qmpmxevc-kvehi-ikk-gsrxemrqirx-750[ojirk]
iuxxuyobk-vrgyzoi-mxgyy-jkbkruvsktz-878[vdepk]
tcorcikpi-wpuvcdng-lgnnadgcp-gpikpggtkpi-362[hiaqt]
sedikcuh-whqtu-fbqijys-whqii-qdqboiyi-114[mhlay]
wdjcvuvmyjpn-agjrzm-mznzvmxc-603[njtzy]
dwbcjkun-snuuhknjw-mnbrpw-771[nuwbj]
qjopwxha-zua-nawymqeoepekj-264[aejop]
nvrgfezqvu-srjbvk-ljvi-kvjkzex-815[vjker]
wpuvcdng-rncuvke-itcuu-fgrnqaogpv-284[ucgnv]
nchhg-jiasmb-uizsmbqvo-408[jnagu]
kpvgtpcvkqpcn-gii-ewuvqogt-ugtxkeg-986[gkptv]
ykjoqian-cnwza-ywjzu-hkceopeyo-576[eqdiy]
excdklvo-nio-wkbuodsxq-692[odkxb]
wdjcvuvmyjpn-wvnfzo-vivgtndn-681[sokpb]
bkzrrhehdc-dff-rsnqzfd-755[dfrhz]
rdggdhxkt-hrpktcvtg-wjci-gtprfjxhxixdc-713[gtxcd]
jvsvymbs-ibuuf-zopwwpun-721[meksh]
ajyqqgdgcb-pybgmyargtc-cee-mncpyrgmlq-366[gcymq]
elrkdcdugrxv-gbh-wudlqlqj-179[dlgqr]
ide-htrgti-qphzti-hidgpvt-765[ithdg]
excdklvo-cmkfoxqob-rexd-cdybkqo-146[oqmuv]
qzchnzbshud-okzrshb-fqzrr-zmzkxrhr-495[nxcry]
wrs-vhfuhw-fdqgb-frdwlqj-vklsslqj-621[flqsw]
kfg-jvtivk-avccpsvre-ljvi-kvjkzex-659[vkjce]
wkqxodsm-nio-psxkxmsxq-328[xskmo]
ktwbhtvmbox-ietlmbv-zktll-nlxk-mxlmbgz-475[lbmtk]
tpspahyf-nyhkl-kfl-dvyrzovw-461[yfhkl]
ejpanjwpekjwh-bhksan-wjwhuoeo-862[jweha]
dsxxw-djmucp-ylyjwqgq-600[djqwx]
pybgmyargtc-afmamjyrc-rcaflmjmew-262[uxngz]
xekdwvwnzkqo-xwogap-yqopkian-oanreya-758[zsntm]
bjfutsneji-hfsid-htfynsl-tujwfyntsx-567[fstjn]
ugdgjxmd-tskcwl-mkwj-lwklafy-424[euphz]
lnkfaypeha-ydkykhwpa-wymqeoepekj-758[zmvns]
wbhsfbohwcboz-gqojsbusf-vibh-rsgwub-506[nryqk]
bnknqetk-dff-btrsnldq-rdquhbd-885[rxizw]
rwcnawjcrxwju-ljwmh-ldbcxvna-bnaerln-277[nwacj]
wbhsfbohwcboz-xszzmpsob-fsqswjwbu-844[jmrta]
xtwtelcj-rclop-upwwjmply-dstaatyr-509[kfcln]
hqfxxnknji-gzssd-htsyfnsrjsy-515[snfhj]
xcitgcpixdcpa-uadltg-detgpixdch-713[tuqak]
bwx-amkzmb-jiasmb-lmxtwgumvb-850[mbawx]
aoubshwq-gqojsbusf-vibh-rsgwub-948[bsugh]
pbybeshy-sybjre-freivprf-715[slnmt]
oxmeeuruqp-otaoaxmfq-dqmocgueufuaz-326[oqsex]
zsxyfgqj-gzssd-btwpxmtu-541[tbeoi]
tfiifjzmv-avccpsvre-rercpjzj-841[megtl]
ltpedcxots-gpqqxi-prfjxhxixdc-635[dljex]
hcd-gsqfsh-suu-gozsg-974[xzhjm]
raphhxuxts-ytaanqtpc-gtrtxkxcv-453[mkcvd]
bkzrrhehdc-bzmcx-lzqjdshmf-313[pzucm]
lhkhszqx-fqzcd-qzaahs-ehmzmbhmf-469[wtdih]
tmrszakd-idkkxadzm-ehmzmbhmf-651[dmzvn]
amppmqgtc-bwc-cleglccpgle-392[cglpe]
yrwxefpi-glsgspexi-eguymwmxmsr-308[egmsx]
jfifqxov-doxab-gbiivybxk-tlohpelm-575[mwlps]
dpssptjwf-qmbtujd-hsbtt-nbslfujoh-181[tlorv]
ftzgxmbv-ietlmbv-zktll-kxtvjnblbmbhg-995[pqmrn]
lxaaxbren-kjbtnc-mnyuxhvnwc-875[sgucv]
ygcrqpkbgf-uecxgpigt-jwpv-fgxgnqrogpv-544[gpcfq]
kwtwznct-lgm-nqvivkqvo-174[qsnxm]
jvuzbtly-nyhkl-jhukf-jbzavtly-zlycpjl-773[ljyzb]
aoubshwq-rms-rsdzcmasbh-766[zqtpb]
jsehsyafy-jsttal-jwsuimakalagf-190[dzuca]
mtzslklcozfd-nlyoj-opawzjxpye-821[lozjp]
eqnqthwn-ecpfa-fgukip-726[oydrm]
bkwzkqsxq-zvkcdsm-qbkcc-domrxyvyqi-848[xycdz]
ugjjgkanw-wyy-dgyaklauk-242[yzsdv]
ugfkmewj-yjsvw-hdsklau-yjskk-vwhsjlewfl-918[badep]
etaqigpke-uecxgpigt-jwpv-fgxgnqrogpv-726[gpeiq]
mvkccspson-mkxni-mykdsxq-wkxkqowoxd-536[cgqjw]
lxwbdvna-pajmn-npp-nwprwnnarwp-563[voqpy]
aflwjfslagfsd-hdsklau-yjskk-vwhdgqewfl-398[madni]
eqttqukxg-ejqeqncvg-yqtmujqr-414[spjrg]
laffe-vrgyzoi-mxgyy-jkyomt-956[tmyzs]
ajyqqgdgcb-hcjjwzcyl-pcqcypaf-834[pmnar]
jrncbavmrq-fpniratre-uhag-qrcyblzrag-247[iynma]
lahxpnwrl-snuuhknjw-nwprwnnarwp-849[mtzsn]
dzczkrip-xiruv-treup-tfrkzex-crsfirkfip-139[szduj]
oazegyqd-sdmpq-oxmeeuruqp-nmewqf-iadwetab-378[zyrxh]
ubhatstkwhnl-ietlmbv-zktll-vnlmhfxk-lxkobvx-553[mnfye]
xfbqpojafe-kfmmzcfbo-usbjojoh-805[tnvco]
bkwzkqsxq-bkllsd-ecob-docdsxq-224[oqnvz]
bjfutsneji-jll-yjhmstqtld-671[jltsb]
bnqqnrhud-idkkxadzm-knfhrshbr-131[ngsmj]
ixccb-elrkdcdugrxv-fdqgb-frdwlqj-uhdftxlvlwlrq-101[bcdyz]
xmtjbzidx-xjinphzm-bmvyz-xviyt-mzvxlpdndodji-239[nmkuv]
udskkaxawv-hdsklau-yjskk-umklgewj-kwjnauw-268[ivymz]
gbc-frperg-wryylorna-znantrzrag-715[ragny]
nzydfxpc-rclop-qwzhpc-wlmzclezcj-145[tbvmx]
dzczkrip-xiruv-irsszk-rercpjzj-841[oyhef]
xcitgcpixdcpa-eaphixr-vgphh-gtrtxkxcv-219[yxomp]
wbhsfbohwcboz-qvcqczohs-qighcasf-gsfjwqs-350[nzxwy]
bpvctixr-tvv-uxcpcrxcv-921[lhyge]
pbeebfvir-cynfgvp-tenff-jbexfubc-949[fbecn]
oazegyqd-sdmpq-nmewqf-dqoquhuzs-742[timug]
mvkccspson-tovvilokx-vklybkdybi-328[mqdsy]
wihmogyl-aluxy-yaa-lymyulwb-864[cdyjz]
ynukcajey-acc-yqopkian-oanreya-836[ciysv]
kyelcrga-afmamjyrc-jyzmpyrmpw-756[myarc]
ovbunmneqbhf-onfxrg-fuvccvat-247[dicga]
kwzzwaqdm-zijjqb-uizsmbqvo-252[zqbij]
eqpuwogt-itcfg-hnqygt-ucngu-414[gtucn]
gpsxdprixkt-qjccn-hwxeexcv-167[xcepd]
aczupnetwp-qwzhpc-opdtry-847[pctwz]
zlilocri-mixpqfz-doxpp-pefmmfkd-575[zwkxc]
zloolpfsb-molgbzqfib-yrkkv-ixyloxqlov-783[ztpiu]
dsxxw-qaytclecp-fslr-nspafyqgle-990[swktj]
tcorcikpi-hnqygt-fgukip-492[icgkp]
bnknqetk-bnqqnrhud-qzaahs-rsnqzfd-365[blisk]
xgjougizobk-inuiurgzk-gtgreyoy-176[mjtsi]
bqvvu-nwxxep-opknwca-836[npvwx]
zixppfcfba-ciltbo-pbosfzbp-367[aritf]
oaxadrgx-dmnnuf-mocgueufuaz-898[egbaz]
sno-rdbqds-dff-cdoknxldms-261[nzdys]
gpewwmjmih-fyrrc-jmrergmrk-204[rmegj]
pyknyegle-aylbw-amyrgle-bcnjmwkclr-184[lyeab]
frqvxphu-judgh-fkrfrodwh-vhuylfhv-751[osurb]
oaddaeuhq-pkq-pqbxakyqzf-560[qadkp]
emixwvqhml-xtiabqk-oziaa-lmaqov-694[aimql]
xlrypetn-awldetn-rcldd-opalcexpye-327[orxhy]
ftzgxmbv-cxeeruxtg-vnlmhfxk-lxkobvx-189[xvbef]
ynssr-vtgwr-xgzbgxxkbgz-501[pfkur]
hdgdovmt-bmvyz-xviyt-xjvodib-vivgtndn-915[sztpg]
dszphfojd-sbccju-tbmft-597[hcsfg]
lxuxaodu-ajkkrc-bcxajpn-823[gbmhy]
ktiaaqnqml-ntwemz-ikycqaqbqwv-694[jnmzc]
fydelmwp-nlyoj-xlylrpxpye-171[nxjhg]
kgjgrypw-epybc-pyzzgr-qcptgacq-522[dxcbq]
fnjyxwrinm-ljwmh-lxjcrwp-lxwcjrwvnwc-225[rypmf]
qfkkj-qwzhpc-qtylyntyr-353[xdymf]
szfyrqriuflj-tfejldvi-xiruv-upv-jyzggzex-555[tsjbl]
jlidywncfy-dyffsvyuh-omyl-nymncha-422[zewrd]
ibghopzs-qobrm-gsfjwqsg-298[yzrtb]
molgbzqfib-zxkav-zlxqfkd-cfkxkzfkd-211[ynduw]
mfklstdw-uzgugdslw-ghwjslagfk-372[ckxrl]
wlqqp-wcfnvi-kirzezex-139[axnvq]
dzczkrip-xiruv-gcrjkzt-xirjj-ivjvrity-711[ezhxy]
mhi-lxvkxm-utldxm-lxkobvxl-501[sflto]
vcibutulxiom-mwupyhayl-bohn-qilembij-734[mfszw]
bjfutsneji-hmthtqfyj-ijxnls-671[fhbgw]
mybbycsfo-oqq-oxqsxoobsxq-354[hlsuv]
nwilwcejc-nwxxep-zalwnpiajp-940[wnpac]
cxy-bnlanc-kjbtnc-dbna-cnbcrwp-589[bodjt]
htwwtxnaj-hfsid-xmnuunsl-931[komlw]
qfkkj-prr-dpcgtnpd-847[tfmev]
vdzonmhydc-bnqqnrhud-cxd-rzkdr-547[zrvqc]
frqvxphu-judgh-exqqb-vklsslqj-231[yehnz]
zlilocri-bdd-abpfdk-913[iraty]
jqwpihizlwca-kpwkwtibm-uiviomumvb-408[iwmbk]
tyepcyletzylw-dnlgpyrpc-sfye-dstaatyr-457[dtrmn]
tcfkqcevkxg-dwppa-yqtmujqr-258[gqsxw]
hdgdovmt-bmvyz-zbb-nvgzn-343[ipyzq]
lxaaxbren-ajkkrc-mnenuxyvnwc-537[naxce]
qzchnzbshud-eknvdq-vnqjrgno-859[porxq]
iutyaskx-mxgjk-pkrrehkgt-jkyomt-904[ktgjm]
dsxxw-aylbw-amyrgle-sqcp-rcqrgle-678[vista]
rflsjynh-gzssd-wjfhvznxnynts-853[nsfhj]
ibghopzs-pibbm-rsdzcmasbh-688[bshim]
pbafhzre-tenqr-cynfgvp-tenff-ratvarrevat-377[fgreq]
diozmivodjivg-ezggtwzvi-rjmfncjk-317[npyji]
jsehsyafy-bwddqtwsf-esfsywewfl-632[baclh]
yhtwhnpun-jhukf-jvhapun-jbzavtly-zlycpjl-149[hjlnp]
lnkfaypeha-iehepwnu-cnwza-ydkykhwpa-ykjpwejiajp-706[mnsri]
oxjmxdfkd-avb-obzbfsfkd-705[nmorj]
sxdobxkdsyxkv-mkxni-zebmrkcsxq-276[jqrnw]
xmtjbzidx-mvhkvbdib-agjrzm-xjiovdihzio-733[ibdjm]
xgvnndadzy-xviyt-omvdidib-863[xzgmn]
pejji-mkxni-crszzsxq-224[stivm]
tpspahyf-nyhkl-yhtwhnpun-ibuuf-zlycpjlz-669[hdrst]
zhdsrqlchg-fkrfrodwh-vwrudjh-179[lneqz]
aczupnetwp-qwzhpc-opgpwzaxpye-145[mbxzs]
dzczkrip-xiruv-wcfnvi-uvgcfpdvek-659[gfzeb]
buzahisl-msvdly-ylhjxbpzpapvu-123[iyjzf]
vetllbybxw-vtgwr-vhtmbgz-kxvxbobgz-969[mavkd]
qfkkj-nlyoj-nzyeltyxpye-119[yejkl]
ynssr-vahvhetmx-hixktmbhgl-969[sqpin]
udglrdfwlyh-gbh-ghsduwphqw-621[znqev]
zilqwikbqdm-jcvvg-wxmzibqwva-226[ohfek]
willimcpy-yaa-nluchcha-292[aclhi]
nvrgfezqvu-avccpsvre-ljvi-kvjkzex-763[vecjk]
vkppo-zubboruqd-ixyffydw-504[bykjv]
laffe-hatte-zxgototm-358[taefo]
iuruxlar-jek-iayzuskx-ykxboik-436[wiqzo]
nij-mywlyn-vohhs-womnigyl-mylpcwy-188[ylmnw]
ksodcbwnsr-rms-kcfygvcd-168[yriva]
zilqwikbqdm-kivlg-kwibqvo-tijwzibwzg-980[sncfm]
wsvsdkbi-qbkno-lexxi-mecdywob-cobfsmo-172[pzjhg]
laffe-xghhoz-zxgototm-722[ofght]
nzwzcqfw-nlyoj-pyrtyppctyr-457[jzkil]
uzfqdzmfuazmx-ngzzk-pqbmdfyqzf-378[zfmqd]
ymszqfuo-omzpk-oamfuzs-mocgueufuaz-664[zvhru]
ibghopzs-qvcqczohs-sbuwbssfwbu-350[xmhnj]
cqwdujys-sqdto-seqjydw-mehaixef-348[rwone]
irdgrxzex-irsszk-rercpjzj-659[ftmuq]
amppmqgtc-pyzzgr-pcqcypaf-418[pcagm]
ygcrqpkbgf-tcddkv-fgrnqaogpv-336[szemt]
hjgbwuladw-uzgugdslw-ugflsafewfl-736[armzs]
mbiyqoxsm-mkxni-mykdsxq-domrxyvyqi-328[mxyiq]
willimcpy-vohhs-qilembij-968[ilhmb]
houngfgxjuay-pkrrehkgt-vaxingyotm-930[cnlzy]
ugfkmewj-yjsvw-tskcwl-ugflsafewfl-320[fwlse]
tpspahyf-nyhkl-msvdly-jvuahputlua-825[ahlpu]
qyujihctyx-wbiwifuny-guleyncha-708[imhls]
qczcftiz-pogysh-twbobqwbu-194[xpmrg]
vetllbybxw-xzz-ltexl-969[yzbur]
kdijqrbu-fbqijys-whqii-jhqydydw-556[iqdjy]
oaddaeuhq-rxaiqd-dqeqmdot-560[xtzsw]
kfg-jvtivk-sleep-fgvirkzfej-321[efkvg]
zhdsrqlchg-udeelw-xvhu-whvwlqj-595[tnkyw]
vetllbybxw-ktuubm-etuhktmhkr-787[tbkue]
mybbycsfo-cmkfoxqob-rexd-ckvoc-224[cmktd]
myxcewob-qbkno-oqq-nocsqx-328[fpyew]
nwilwcejc-fahhuxawj-zalhkuiajp-810[askzg]
ovbunmneqbhf-pnaql-ynobengbel-377[nkafy]
qzchnzbshud-okzrshb-fqzrr-nodqzshnmr-677[mnude]
bdavqofuxq-fab-eqodqf-dmnnuf-ogefayqd-eqdhuoq-586[xmztn]
rdchjbtg-vgpst-ytaanqtpc-hpath-531[xmzrt]
vetllbybxw-wrx-mktbgbgz-735[ijzlh]
zvyvgnel-tenqr-pnaql-fuvccvat-689[vnace]
muqfedyput-sqdto-sedjqydcudj-478[dquej]
lahxpnwrl-mhn-jlzdrbrcrxw-147[psbch]
eadalsjq-yjsvw-jsttal-vwnwdghewfl-346[wajls]
hafgnoyr-sybjre-qrfvta-247[rafyb]
votubcmf-tdbwfohfs-ivou-dpoubjonfou-129[zibsh]
jrncbavmrq-pnaql-pbngvat-hfre-grfgvat-143[argnv]
tcrjjzwzvu-sleep-nfibjyfg-555[jefzb]
myxcewob-qbkno-bkllsd-bokmaescsdsyx-406[ghwzx]
cqwdujys-fbqijys-whqii-udwyduuhydw-972[hdvyx]
xqvwdeoh-edvnhw-zrunvkrs-439[jqtmz]
bqvvu-ydkykhwpa-zalhkuiajp-108[xmkdz]
gzefmnxq-eomhqzsqd-tgzf-mzmxkeue-534[emzqf]
zloolpfsb-gbiivybxk-zrpqljbo-pbosfzb-939[symnz]
nzwzcqfw-awldetn-rcldd-cpdplcns-717[cdlnw]
ide-htrgti-rpcsn-rdpixcv-ldgzhwde-661[dirce]
fkqbokxqflkxi-bdd-qoxfkfkd-367[kfdqx]
jlidywncfy-vumeyn-jolwbumcha-214[shtrx]
vagreangvbany-onfxrg-ernpdhvfvgvba-429[shgyt]
odkasqzuo-pkq-xmnadmfadk-742[adkmo]
mvydjvxodqz-xviyt-xjvodib-zibdizzmdib-889[divzb]
dpssptjwf-dboez-eftjho-467[ajfyk]
zilqwikbqdm-xtiabqk-oziaa-kcabwumz-amzdqkm-486[clwnt]
plolwdub-judgh-sodvwlf-judvv-uhfhlylqj-699[trqoi]
bnqqnrhud-rbzudmfdq-gtms-nodqzshnmr-885[dnqmr]
atyzghrk-hatte-vaxingyotm-800[nyzck]
amppmqgtc-afmamjyrc-bctcjmnkclr-730[jbafl]
tpspahyf-nyhkl-yhiipa-svnpzapjz-617[xijwv]
dkqjcbctfqwu-ecpfa-tgugctej-908[ctefg]
hcd-gsqfsh-tzcksf-rsjszcdasbh-896[ayzso]
bknsykmdsfo-lexxi-myxdksxwoxd-380[hxsvz]
vetllbybxw-xzz-tvjnblbmbhg-319[htivo]
bpvctixr-qjccn-hpath-349[kxwzv]
szfyrqriuflj-treup-tfrkzex-cfxzjkztj-347[skbiw]
kwvacumz-ozilm-lgm-lmxtwgumvb-876[gcnyk]
fmsledevhsyw-fyrrc-wivzmgiw-620[mwkyz]
bnmrtldq-fqzcd-bgnbnkzsd-rsnqzfd-781[eiqgm]
amlqskcp-epybc-aylbw-amyrgle-jmegqrgaq-756[uygno]
foadouwbu-xszzmpsob-hsqvbczcum-792[xrtpc]
vjpwncrl-fnjyxwrinm-kdwwh-bcxajpn-719[nwjcp]
enqvbnpgvir-cynfgvp-tenff-qrfvta-195[cpvnk]
buzahisl-lnn-klwhyatlua-851[tmzna]
shmml-enoovg-bcrengvbaf-533[begmn]
pdjqhwlf-hjj-whfkqrorjb-257[jhfqr]
wfintfhynaj-gzssd-wjhjnansl-307[njsaf]
qczcftiz-rms-kcfygvcd-376[kybix]
pbeebfvir-sybjre-jbexfubc-975[jzufx]
irdgrxzex-drxevkzt-wcfnvi-fgvirkzfej-191[wfdlg]
irgyyolokj-lruckx-gtgreyoy-930[wnqvm]
lsyrkjkbnyec-mkxni-mykdsxq-ckvoc-276[gwtls]
enzcntvat-sybjre-znexrgvat-741[mixqj]
myxcewob-qbkno-nio-oxqsxoobsxq-666[dracq]
excdklvo-mkxni-mykdsxq-vklybkdybi-172[kdxyb]
xfbqpojafe-tdbwfohfs-ivou-tijqqjoh-103[mjxla]
rkpqxyib-mixpqfz-doxpp-obzbfsfkd-341[eswqm]
pualyuhapvuhs-wshzapj-nyhzz-thuhnltlua-695[whgcf]
tyepcyletzylw-awldetn-rcldd-nfdezxpc-dpcgtnp-535[dcelp]
dpotvnfs-hsbef-cvooz-eftjho-909[wrstm]
esyfwlau-wyy-jwsuimakalagf-658[cbdnm]
luxciuwncpy-vumeyn-mbcjjcha-396[cujmn]
udskkaxawv-jsttal-vwhsjlewfl-268[alswj]
ckgvutofkj-xghhoz-xkikobotm-410[koght]
kgjgrypw-epybc-njyqrga-epyqq-pcyaosgqgrgml-626[atdyg]
cybyjqho-whqtu-ryexqpqhteki-tou-vydqdsydw-998[bnjht]
froruixo-udeelw-ghvljq-127[bylzc]
forwcoqhwjs-tinnm-gqojsbusf-vibh-fsgsofqv-974[sxtko]
wsvsdkbi-qbkno-mkxni-dbksxsxq-432[ymxdz]
ryexqpqhteki-cybyjqho-whqtu-sqdto-jusxdebewo-634[qehot]
yrwxefpi-hci-wxsveki-750[iewxc]
ugjjgkanw-ujqgywfau-tmffq-hmjuzskafy-788[alfsc]
jef-iushuj-sxesebqju-kiuh-juijydw-114[jueis]
lsyrkjkbnyec-excdklvo-pvygob-yzobkdsyxc-198[ykbco]
eadalsjq-yjsvw-ugjjgkanw-xdgowj-wfyafwwjafy-866[miqfs]
etaqigpke-ecpfa-qrgtcvkqpu-466[qdvna]
frqvxphu-judgh-xqvwdeoh-mhoobehdq-pdqdjhphqw-283[zjdiv]
frqvxphu-judgh-vfdyhqjhu-kxqw-hqjlqhhulqj-283[mnykz]
hjgbwuladw-usfvq-ugslafy-jwkwsjuz-450[mitsc]
rkpqxyib-avb-abpfdk-575[bakpd]
fubrjhqlf-fkrfrodwh-oderudwrub-959[vdxlz]
sgmtkzoi-pkrrehkgt-rumoyzoiy-436[korgi]
hwdtljsnh-idj-wjhjnansl-801[fobmn]
ftzgxmbv-unggr-inkvatlbgz-423[ecjzy]
jrncbavmrq-pnaql-pbngvat-znantrzrag-585[anrbg]
sehheiylu-muqfedyput-rqiauj-tulubefcudj-530[eadkp]
odkasqzuo-pkq-pqhqxabyqzf-950[iyzwg]
lujbbrornm-vjpwncrl-kdwwh-ydalqjbrwp-615[lmjzy]
kpvgtpcvkqpcn-dwppa-rwtejcukpi-336[pcktv]
hwbba-gii-eqpvckpogpv-492[pbgiv]
zsxyfgqj-hqfxxnknji-idj-xytwflj-359[jxfin]")

(defn checksum [name]
  (apply str (take 5 (map first (sort (fn [[l n] [l' n']] (or (> n n') (and (= n n') (< (int l) (int l'))))) (-> name frequencies (dissoc \-)))))))

(defn day4-1 [input]
  (->> (re-seq #"([-a-z]+)-(\d+)\[([a-z]+)\]" input)
    (filter (fn [[_ name sector-id sum]] (= (checksum name) sum)))
    (map (fn [[_ name sector-id sum]] (Long/parseLong sector-id)))
    (reduce +)))

(defn decrypt [[_ name sector-id]]
  (let [a (int \a)] 
    [(apply str 
       (map
         (fn [c]
           (if (= \- c)
             \space
             (char (+ a (mod (+ (int c) (- a) (Long/parseLong sector-id)) 26)))))
         name))
     sector-id]))

(defn day4-2 [input]
  (->> (re-seq #"([-a-z]+)-(\d+)\[([a-z]+)\]" input)
    (filter (fn [[_ name sector-id sum]] (= (checksum name) sum)))
    (map decrypt)
    (filter (fn [[name]] (re-matches #".*pole.*" name)))))

;; day 5
(def day5-input "cxdnnyjw")

(defn md5 [^String s]
  (apply str (map #(format "%02x" %) (.digest (java.security.MessageDigest/getInstance "MD5") (.getBytes s "UTF8")))))

(defn day5-1 [input] 
  (->> (map #(md5 (str input %)) (range))
    (filter #(.startsWith % "00000"))
    (map #(nth % 5))
    (take 8)
    (apply str)))

(defn day5-2 [input] 
  (transduce (comp (map #(md5 (str input %)))
               (filter #(.startsWith ^String % "00000")))
    (fn
      ([code] (apply str code))
      ([code md5]
        (let [pos (- (int (nth md5 5)) (int \0))]
          (if (= (nth code pos nil) \_)
            (let [code (assoc code pos (nth md5 6))]
              (if (some #{\_} code) code (reduced code)))
            code))))
    (vec "________")
    (range)))