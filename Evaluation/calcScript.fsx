
// NOW STARTING WITH ASSERTIONS in the CODE (SH)       
let SHa = [922 ;917 ;943 ;957 ;925 ;917 ;961 ;971 ;940 ;938 ;931 ;944 ;938; 938; 988; 932; 958; 961; 921] |> List.averageBy float
// 942.2105263


// This is the SH with assertions 
let SHass = [962 ;958 ;948 ;947 ;961 ;937 ;928 ;960 ;980 ;930 ;951 ;977 ] |> List.averageBy float
// 953.25

// NOW STARTING WITHOUT ASSERTIONS in the CODE (SH)       
let SHb = [949  ;924 ;935 ; 939 ;933 ;925 ;929 ;948 ;958 ;970 ;928 ;931 ;938 ;982 ] |> List.averageBy float
// 946.9333333

// SH TCP 
let SHTCP = [896 ;885 ;948 ;903 ;909 ;878 ;921 ;897 ;903 ;899 ;886 ;896 ;908 ;886 ] |> List.averageBy float
// 901.0714286


// ===== OTHER=====
// Otyher results with TP (check them, smth is off, think with ass but three party and printing)
let tp = [1064 ;1002 ;985 ;1019 ;1073 ;1031 ;1030 ;1070] |> List.averageBy float
// 1034.25

// SH WITH ASSERTIONS in TP (shoudl retake)
let SHc = [940 ;913 ;938 ;917; 939 ;932; 936 ;922 ;1019 ;957 ;913 ;965] |> List.averageBy float


// 940 
// ===== OTHER=====

//============List Sorting===========
// TP with assertions
let Sa = [12147 ; 12042; 12304; 11709 ] |> List.averageBy float 
//12050

// TP without assertions
let Sb = [11956;11709; 12194;10884 ] |> List.averageBy float 
// 11685

// TCP 
let Sc = [10884; 10952; 11286; 10912 ] |> List.averageBy float 
// 11008


//============Gen Rec===========
// TP no asertions
let GR0 = [1023 ;1025 ;1016 ;1014 ;1061] |> List.averageBy float 
// 1027.8

// TP with asertions
let GRa = [1177 ;1156 ;1148 ;1184 ;1162] |> List.averageBy float 
// 1165.4

// TP asertions in the code 
let GRb = [1067 ;1010 ;1009 ;1019 ;1022 ;1038 ] |> List.averageBy float 
// 1027.5

// TCP
let GRc = [957 ;980 ;959 ;960 ;1044 ;970 ;997 ;976 ] |> List.averageBy float 
// 980.375


//============Gen 10000===========

// No assertions at all 
let Gen0 = [2885 ;2868 ;2892 ;2895 ;2892 ;2896;2885 ;2873 ;2853 ;3007] |> List.averageBy float 
// 2894.6

// TP with Assertions
let Gena = [3524 ;3265 ;3234 ;3294 ;3250 ;3257 ;3275 ;3254 ;3258 ;3487 ] |> List.averageBy float 
// 3309.8

// TP assertions in the code
let Genb = [2901 ;2906 ;2929 ;2865 ;2848 ;2906 ;2906 ;2903 ;3046 ;2919 ] |> List.averageBy float 
//2912.9

// TCP
let Genc = [2500 ;2537 ;2493 ;2533 ;2618 ;2560 ;2519 ;2566 ;2580 ;2507 ;2541 ;2556 ] |> List.averageBy float 
//2542.5

let all = List.empty
let t100 = [1022;1024 ] |> List.averageBy float 
let t90 = [921 ; 921 ; 932 ] |> List.averageBy float
let t80 = [824 ; 848 ; 865 ] |> List.averageBy float
let t70 = [746 ; 770  ] |> List.averageBy float
let t60 = [630] |> List.averageBy float
let t50 =  [589] |>   List.averageBy float
let t40 = [488 ; 483 ] |>   List.averageBy float
let t30 = [396; 403] |>   List.averageBy float
let t20 = [295; 296 ]  |>   List.averageBy float
let t10 = [192; 205; 218 ]  |>   List.averageBy float

// ================= Gen REc 100 iterations * n  repetisions ===============================// 
let seq_assertions_in_the_code = [t100; t90; t80; t70; t60; t50; t40; t30; t20; t10;]|> List.rev
//  [205.0; 295.5; 399.5; 485.5; 589.0; 630.0; 758.0; 845.6666667; 924; 1023.0]
// [1023.0; 845.6666667; 845.6666667; 758.0; 630.0; 589.0; 485.5; 399.5; 295.5;205.0]

let a100 = [1164;1174 ] |> List.averageBy float 
let a90 = [1059;1174 ] |> List.averageBy float 
let a80 = [951;944 ] |> List.averageBy float 
let a70 = [866;866;864] |> List.averageBy float 
let a60 = [746; 843] |> List.averageBy float 
let a50 = [661 ; 652] |> List.averageBy float 
let a40 = [525; 546 ] |> List.averageBy float 
let a30 = [439; 424 ] |> List.averageBy float 
let a20 = [325; 319; 327] |> List.averageBy float 
let a10 = [219; 218 ] |> List.averageBy float 

let seq_assertions_in_TP = [a100; a90; a80; a70; a60; a50; a40; a30; a20; a10;] |> List.rev
 //  [218.5; 323.6666667; 431.5; 535.5; 656.5; 794.5; 865.3333333; 947.5; 1116.5; 1169.0]
// [1169.0; 1116.5; 947.5; 865.3333333; 794.5; 656.5; 535.5; 431.5; 323.6666667; 218.5]


let it_overhead = List.map2 (fun x y -> ((x - y )/y) * 100.0 ) seq_assertions_in_TP seq_assertions_in_the_code

 // [6.585365854; 9.531866892; 8.010012516; 10.29866117; 11.46010187; 26.11111111; 14.16007036; 12.04178163; 20.74621485; 14.27174976]

let c100 = [926; 941  ] |> List.averageBy float 
let c90 = [848; 849 ; 984  ] |> List.averageBy float 
let c80 = [767; 771; 780 ] |> List.averageBy float 
let c70 = [729; 698; 744] |> List.averageBy float 
let c60 = [ 612;  627 ] |> List.averageBy float 
let c50 = [549 ; 546 ; 548 ] |> List.averageBy float 
let c40 = [473 ; 464 ; 462 ] |> List.averageBy float 
let c30 = [385 ; 385 ; 389 ] |> List.averageBy float 
let c20 = [306;  312; 313 ] |> List.averageBy float 
let c10 = [233; 232] |> List.averageBy float 

let seq_assertions_TPC = [c100; c90; c80; c70; c60; c50; c40; c30; c20; c10;] |> List.rev
// [232.5; 310.3333333; 386.3333333; 466.3333333; 547.6666667; 642.6666667; 723.6666667; 772.6666667; 893.6666667; 933.5]



let tcp_overhead = List.map2 (fun x y -> ((x - y )/y) * 100.0 ) seq_assertions_in_the_code seq_assertions_TPC

// [-11.82795699; -4.77980666; 3.40811044; 4.110078628; 7.547169811; 3.694915254; 4.744357439; 9.447799827; 3.468854905; 9.587573648]

let scr = [435 ; 511; 547; 593; 597; 605; 632; 615; 723]

let L = [[51 ; 6; 721]; [72 ; 9; 1100 ]; [15; 1522 ]; [123; 16; 1935 ]; [133; 16 ; 2379 ]; [135 ; 21; 2713]; [151; 27; 3230];[136; 22; 3679] ; [141 ; 57; 3851]]

let tp_overhead = L |> List.map (fun x -> List.sum x)

// [778; 1181; 1537; 2074; 2528; 2869; 3837; 3408; 4049]
let all_overhead = List.map2 (+) scr tp_overhead

// [1213; 1692; 2084; 2667; 3125; 3474; 4452; 4040; 4772]


// === with assertions ==========

let ascr = [1074 ; 1392; 2333; 2920; 3260;3917; 4562; 5395; 5807; 7017]

let aL =  [[23; 20 ; 3534]; [38 ; 15; 4962] ; [229 ; 82 ; 8642]; [84;  11 ; 7292]; [102 ; 27; 9418]; [125; 15 ; 13223] ; [108 ; 16 ; 14307]; [271 ; 69 ; 20440 ]; [124; 43; 21214]; [281; 95; 26998]]
let atp_overhead = aL |> List.map (fun x -> List.sum x)

let aall_overhead = List.map2 (+) ascr atp_overhead
// [4651; 6407; 11286; 10307; 12807; 17280; 18993; 26175; 27188; 34391]


// 50 

let ascr1 = [2918; 2376; 1714; 1239; 805] |> List.rev
[805; 1239; 1714; 2376; 2918]
[1511; 3327; 5376; 7503; 11093]
let aL1 = [[140; 18; 8017]; [39; 89; 4999 ]; [73;29; 3560]; [5; 48;2035]; [11;3;692]]

let overhead_gen = aL1 |> List.map (fun x -> List.sum x) |> List.rev
let finalres = List.map2 (+) ascr1 overhead_gen


// 40 [2101; 2101; ] |> List.averageBy float 



// 20 
After Scribble Compiler: 1239 
After Parsing : 43 
After Type generation: 29 
From the cache: 2063 
From the cache: 655 
Assembly: 0 

After Scribble Compiler: 1250 
After Parsing : 52 
After Type generation: 13 
From the cache: 2953 
From the cache: 751 
Assembly: 0 

After Scribble Compiler: 1195 
After Parsing : 48 
After Type generation: 5 
From the cache: 2035 
From the cache: 760 
Assembly: 0 

// 10 
After Scribble Compiler: 805 
After Parsing : 11 
After Type generation: 3 
From the cache: 692 
From the cache: 473 
Assembly: 0 
After Scribble Compiler: 759 
After Parsing : 11 
After Type generation: 3 
From the cache: 677 
From the cache: 567 





let noIR = [0.9469333333; 0.500;  1.0275]

let withIR = [0.95325; 0.550; 1.1654]

let bareTCP = [0.9010714286; 0.450; 0.980375]

let s = List.map2 (fun x y -> (x - y)/y * 100.0) noIR bareTCP 
let v = List.map2 (fun x y -> (x - y)/y * 100.0) withIR noIR 

SH (0.9469333333-0.9010714286)/ 0.9010714286

(0.95325 - 0.9469333333)/ 0.9469333333

 (0.550 - 0.450)/ 0.450
 50/500

 ( 1.1654 - 1.0275)/1.0275

let noIR = [0.345; 0.695; 1.024; 1.223; 1.472]

let withIR = [0.389;0.693; 1.039; 1.365; 1.626]

let bareTCP = [0.476; 0.705; 0.969; 1.195; 1.442]

let s = List.map2 (fun x y -> (x - y)/y * 100.0) noIR bareTCP 
let v = List.map2 (fun x y -> (x - y)/y * 100.0) withIR noIR  *)

