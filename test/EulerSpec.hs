-- file EulerSpec.hs
module EulerSpec where

import qualified Data.Map as Map
import Debug.Trace
import Euler.Problem001
import Euler.Problem002
import Euler.Problem003
import Euler.Problem004
import Euler.Problem005
import Euler.Problem006
import Euler.Problem007
import Euler.Problem008
import Euler.Problem009
import Euler.Problem010
import Euler.Problem011
import Euler.Problem012
import Euler.Problem013
import Euler.Problem014
import Euler.Problem015
import Euler.Problem016
import Euler.Problem017
import Euler.Problem018
import Euler.Problem019
import Euler.Problem020
import Euler.Problem021
import Euler.Problem022
import Euler.Problem023
import Euler.Problem024
import Euler.Problem025
import Euler.Problem026
import Euler.Problem027
import Euler.Problem028
import Euler.Problem029
import Euler.Problem030
import Euler.Problem031
import Euler.Problem032
import Euler.Problem033
import Euler.Problem034
import Euler.Problem035
import Euler.Problem036 (euler36)
import Euler.Problem037
import Euler.Problem038
import Euler.Problem039
import Euler.Problem040
import Euler.Problem041
import Euler.Problem042
import Euler.Problem043
import Euler.Problem044
import Euler.Problem045
import Euler.Problem046
import Euler.Problem047
import Euler.Problem048
import Euler.Problem049
import Euler.Problem050
import Euler.Problem051
import Euler.Problem052
import Euler.Problem053
import Euler.Problem055
import Euler.Problem056
import Euler.Problem057
import Euler.Problem058
import Euler.Problem059
import Euler.Problem060
import Euler.Problem062
import Fibonacci
import Test.Hspec

problem8number = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"

-- grid = [[11,12,13,14], [21,22,23,24], [31,32,33,34], [41,42,43,44]]
grid =
  [ [08, 02, 22, 97, 38, 15, 00, 40, 00, 75, 04, 05, 07, 78, 52, 12, 50, 77, 91, 08],
    [49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48, 04, 56, 62, 00],
    [81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30, 03, 49, 13, 36, 65],
    [52, 70, 95, 23, 04, 60, 11, 42, 69, 24, 68, 56, 01, 32, 56, 71, 37, 02, 36, 91],
    [22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80],
    [24, 47, 32, 60, 99, 03, 45, 02, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50],
    [32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70],
    [67, 26, 20, 68, 02, 62, 12, 20, 95, 63, 94, 39, 63, 08, 40, 91, 66, 49, 94, 21],
    [24, 55, 58, 05, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72],
    [21, 36, 23, 09, 75, 00, 76, 44, 20, 45, 35, 14, 00, 61, 33, 97, 34, 31, 33, 95],
    [78, 17, 53, 28, 22, 75, 31, 67, 15, 94, 03, 80, 04, 62, 16, 14, 09, 53, 56, 92],
    [16, 39, 05, 42, 96, 35, 31, 47, 55, 58, 88, 24, 00, 17, 54, 24, 36, 29, 85, 57],
    [86, 56, 00, 48, 35, 71, 89, 07, 05, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58],
    [19, 80, 81, 68, 05, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77, 04, 89, 55, 40],
    [04, 52, 08, 83, 97, 35, 99, 16, 07, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66],
    [88, 36, 68, 87, 57, 62, 20, 72, 03, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69],
    [04, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18, 08, 46, 29, 32, 40, 62, 76, 36],
    [20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74, 04, 36, 16],
    [20, 73, 35, 29, 78, 31, 90, 01, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57, 05, 54],
    [01, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52, 01, 89, 19, 67, 48]
  ]

bigsum =
  [ 37107287533902102798797998220837590246510135740250,
    46376937677490009712648124896970078050417018260538,
    74324986199524741059474233309513058123726617309629,
    91942213363574161572522430563301811072406154908250,
    23067588207539346171171980310421047513778063246676,
    89261670696623633820136378418383684178734361726757,
    28112879812849979408065481931592621691275889832738,
    44274228917432520321923589422876796487670272189318,
    47451445736001306439091167216856844588711603153276,
    70386486105843025439939619828917593665686757934951,
    62176457141856560629502157223196586755079324193331,
    64906352462741904929101432445813822663347944758178,
    92575867718337217661963751590579239728245598838407,
    58203565325359399008402633568948830189458628227828,
    80181199384826282014278194139940567587151170094390,
    35398664372827112653829987240784473053190104293586,
    86515506006295864861532075273371959191420517255829,
    71693888707715466499115593487603532921714970056938,
    54370070576826684624621495650076471787294438377604,
    53282654108756828443191190634694037855217779295145,
    36123272525000296071075082563815656710885258350721,
    45876576172410976447339110607218265236877223636045,
    17423706905851860660448207621209813287860733969412,
    81142660418086830619328460811191061556940512689692,
    51934325451728388641918047049293215058642563049483,
    62467221648435076201727918039944693004732956340691,
    15732444386908125794514089057706229429197107928209,
    55037687525678773091862540744969844508330393682126,
    18336384825330154686196124348767681297534375946515,
    80386287592878490201521685554828717201219257766954,
    78182833757993103614740356856449095527097864797581,
    16726320100436897842553539920931837441497806860984,
    48403098129077791799088218795327364475675590848030,
    87086987551392711854517078544161852424320693150332,
    59959406895756536782107074926966537676326235447210,
    69793950679652694742597709739166693763042633987085,
    41052684708299085211399427365734116182760315001271,
    65378607361501080857009149939512557028198746004375,
    35829035317434717326932123578154982629742552737307,
    94953759765105305946966067683156574377167401875275,
    88902802571733229619176668713819931811048770190271,
    25267680276078003013678680992525463401061632866526,
    36270218540497705585629946580636237993140746255962,
    24074486908231174977792365466257246923322810917141,
    91430288197103288597806669760892938638285025333403,
    34413065578016127815921815005561868836468420090470,
    23053081172816430487623791969842487255036638784583,
    11487696932154902810424020138335124462181441773470,
    63783299490636259666498587618221225225512486764533,
    67720186971698544312419572409913959008952310058822,
    95548255300263520781532296796249481641953868218774,
    76085327132285723110424803456124867697064507995236,
    37774242535411291684276865538926205024910326572967,
    23701913275725675285653248258265463092207058596522,
    29798860272258331913126375147341994889534765745501,
    18495701454879288984856827726077713721403798879715,
    38298203783031473527721580348144513491373226651381,
    34829543829199918180278916522431027392251122869539,
    40957953066405232632538044100059654939159879593635,
    29746152185502371307642255121183693803580388584903,
    41698116222072977186158236678424689157993532961922,
    62467957194401269043877107275048102390895523597457,
    23189706772547915061505504953922979530901129967519,
    86188088225875314529584099251203829009407770775672,
    11306739708304724483816533873502340845647058077308,
    82959174767140363198008187129011875491310547126581,
    97623331044818386269515456334926366572897563400500,
    42846280183517070527831839425882145521227251250327,
    55121603546981200581762165212827652751691296897789,
    32238195734329339946437501907836945765883352399886,
    75506164965184775180738168837861091527357929701337,
    62177842752192623401942399639168044983993173312731,
    32924185707147349566916674687634660915035914677504,
    99518671430235219628894890102423325116913619626622,
    73267460800591547471830798392868535206946944540724,
    76841822524674417161514036427982273348055556214818,
    97142617910342598647204516893989422179826088076852,
    87783646182799346313767754307809363333018982642090,
    10848802521674670883215120185883543223812876952786,
    71329612474782464538636993009049310363619763878039,
    62184073572399794223406235393808339651327408011116,
    66627891981488087797941876876144230030984490851411,
    60661826293682836764744779239180335110989069790714,
    85786944089552990653640447425576083659976645795096,
    66024396409905389607120198219976047599490197230297,
    64913982680032973156037120041377903785566085089252,
    16730939319872750275468906903707539413042652315011,
    94809377245048795150954100921645863754710598436791,
    78639167021187492431995700641917969777599028300699,
    15368713711936614952811305876380278410754449733078,
    40789923115535562561142322423255033685442488917353,
    44889911501440648020369068063960672322193204149535,
    41503128880339536053299340368006977710650566631954,
    81234880673210146739058568557934581403627822703280,
    82616570773948327592232845941706525094512325230608,
    22918802058777319719839450180888072429661980811197,
    77158542502016545090413245809786882778948721859617,
    72107838435069186155435662884062257473692284509516,
    20849603980134001723930671666823555245252804609722,
    53503534226472524250874054075591789781264330331690
  ]

problem18triangle =
  [ [75],
    [95, 64],
    [17, 47, 82],
    [18, 35, 87, 10],
    [20, 04, 82, 47, 65],
    [19, 01, 23, 75, 03, 34],
    [88, 02, 77, 73, 07, 63, 67],
    [99, 65, 04, 28, 06, 16, 70, 92],
    [41, 41, 26, 56, 83, 40, 80, 70, 33],
    [41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
    [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
    [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
    [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
    [63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
    [04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23]
  ]

spec :: Spec
spec = do
  describe "euler01" $ do
    it "sums multiples below 10" $
      euler01 10 `shouldBe` 23
    it "sums multiples below 1000" $
      euler01 1000 `shouldBe` 233168
  describe "fibs" $ do
    it "generates Fibonacci sequence" $
      fibs 10 `shouldBe` reverse [1, 2, 3, 5, 8, 13, 21, 34, 55, 89]
  describe "euler02" $ do
    it "returns example" $
      euler02 90 `shouldBe` 44
    it "returns solution" $
      euler02 4000000 `shouldBe` 4613732

  describe "euler03" $ do
    it "returns example" $
      euler03 13195 `shouldBe` [5, 7, 13, 29]
    it "returns solution" $
      euler03 600851475143 `shouldBe` [71, 839, 1471, 6857]

  describe "euler04" $ do
    it "returns example" $
      euler04 [10 .. 99] `shouldBe` 9009
    it "returns solution" $
      euler04 [100 .. 999] `shouldBe` 906609

  describe "euler05" $ do
    it "returns example" $
      euler05 10 `shouldBe` 2520
    it "returns solution" $
      euler05 20 `shouldBe` 232792560

  describe "euler06" $ do
    it "returns example" $
      euler06 10 `shouldBe` 2640
    it "returns solution" $
      euler06 100 `shouldBe` 25164150

  describe "euler07" $ do
    it "returns example" $
      euler07 6 `shouldBe` 13
    it "returns solution" $
      euler07 10001 `shouldBe` 104743

  describe "euler08" $ do
    it "returns example" $
      euler08 problem8number 4 `shouldBe` 5832
    it "returns solution" $
      euler08 problem8number 13 `shouldBe` 23514624000

  describe "euler09" $ do
    it "returns example" $
      euler09 (3 + 4 + 5) `shouldBe` (3, 4, 5)
    it "returns solution" $
      euler09 1000 `shouldBe` (200, 375, 425)

  describe "euler10" $ do
    it "returns example" $
      euler10 10 `shouldBe` 17
    xit "returns solution" $
      euler10 2000000 `shouldBe` 142913828922

  describe "euler11" $ do
    it "returns solution" $
      euler11 4 grid `shouldBe` 70600674

  describe "euler12" $ do
    it "returns first term" $
      euler12 1 `shouldBe` 3
    it "returns second term" $
      euler12 2 `shouldBe` 6
    it "returns third term" $
      euler12 4 `shouldBe` 28
    it "returns example" $
      euler12 5 `shouldBe` 28
    xit "returns solution" $
      euler12 500 `shouldBe` 76576500

  describe "euler13" $ do
    it "returns solution" $
      euler13 bigsum `shouldBe` "5537376230"

  describe "euler14" $ do
    it "returns first term" $
      euler14 1 `shouldBe` 1
    it "returns second term" $
      euler14 2 `shouldBe` 2
    it "returns third term" $
      euler14 3 `shouldBe` 3
    xit "returns solution" $
      euler14 1000000 `shouldBe` 837799

  describe "euler15" $ do
    it "returns example" $
      euler15 2 `shouldBe` 6
    it "returns solution" $
      euler15 20 `shouldBe` 137846528820

  describe "euler16" $ do
    it "returns example" $
      euler16 15 `shouldBe` 26
    it "returns solution" $
      euler16 1000 `shouldBe` 1366

  describe "euler17" $ do
    it "returns example" $
      euler17 [342, 115] `shouldBe` [43]
    it "returns example" $
      euler17 [1, 19, 20, 23, 99, 132, 200, 899, 1000] `shouldBe` [106]
    it "returns solution" $
      euler17 [1 .. 1000] `shouldBe` [21124]

  describe "euler18" $ do
    it "returns example" $
      euler18 [[3], [7, 4], [2, 4, 6], [8, 5, 9, 3]] `shouldBe` 23
    it "returns example" $
      euler18 [[3], [7, 4], [2, 4, 6], [8, 5, 9, 3], [9, 1, 1, 1, 1]] `shouldBe` 29
    it "returns solution" $
      euler18 problem18triangle `shouldBe` 1074

  describe "euler19" $ do
    it "returns example" $
      euler19 `shouldBe` 171

  describe "euler20" $ do
    it "returns example" $
      euler20 10 `shouldBe` 27
    it "returns solution" $
      euler20 100 `shouldBe` 648

  describe "euler21" $ do
    describe "d" $ do
      it "returns example" $
        d 220 `shouldBe` 284
      it "returns example" $
        d 284 `shouldBe` 220
    it "returns solution" $
      euler21 10000 `shouldBe` 31626

  describe "euler22" $ do
    it "returns solution" $ do
      names <- readFile "names.txt"
      euler22 names `shouldBe` [871198282]

  describe "euler23" $ do
    xit "returns solution" $
      euler23 28123 `shouldBe` 123

  describe "euler24" $ do
    it "returns example" $
      euler24 [0 .. 2] `shouldBe` ["012", "021", "102", "120", "201", "210"]
    it "returns example" $
      euler24 [0 .. 3] `shouldBe` euler24' [0 .. 3]
    xit "returns solution" $
      (euler24' [0 .. 9] !! (1000000 -1)) `shouldBe` "2783915460"

  describe "euler25" $ do
    it "returns example" $
      euler25 3 `shouldBe` [12]
    it "returns solution" $
      euler25 1000 `shouldBe` [4782]

  describe "euler26" $ do
    it "returns example" $
      euler26 10 `shouldBe` (7, 6)
    it "returns solution" $
      euler26 999 `shouldBe` (983, 982)

  describe "euler27" $ do
    it "returns example" $
      euler27 41 `shouldBe` [(-1, 41, 41)]
    it "returns solution" $
      euler27 1000 `shouldBe` [(-61, 971, 71)]

  describe "euler28" $ do
    it "returns example" $
      euler28 5 `shouldBe` 101
    it "returns solution" $
      euler28 1001 `shouldBe` 669171001

  describe "euler29" $ do
    it "returns example" $
      euler29 5 `shouldBe` [15]
    it "returns solution" $
      euler29 100 `shouldBe` [9183]

  describe "euler30" $ do
    it "returns example" $
      euler30 4 `shouldBe` [1634, 8208, 9474]
  it "returns solution" $
    euler30 5 `shouldBe` [4150, 4151, 54748, 92727, 93084, 194979]

  describe "euler31" $ do
    it "returns example" $
      euler31 10 `shouldBe` 11
    it "returns solution" $
      euler31 200 `shouldBe` 73682

  describe "euler32" $ do
    it "returns solution" $
      euler32 [1 .. 9] `shouldBe` [45228]

  describe "euler33" $ do
    it "returns solution" $
      euler33 `shouldBe` 100

  describe "euler34" $ do
    xit "returns solution" $
      euler34 `shouldBe` [145, 40585]

  describe "euler35" $ do
    it "returns example" $
      euler35 100 `shouldBe` [13]
    it "returns example" $
      euler35 1000 `shouldBe` [25]
    it "returns solution" $
      euler35 1000000 `shouldBe` [55]

  describe "euler36" $ do
    it "returns solution" $
      euler36 1000000 `shouldBe` [872187]

  describe "euler37" $ do
    it "returns solution" $
      euler37 `shouldBe` [748317]

  describe "euler38" $ do
    xit "returns solution" $
      euler38 `shouldBe` [(9327, 2, 932718654)]

  describe "euler39" $ do
    it "returns solution" $
      euler39 12 `shouldBe` [12]
    it "returns solution" $
      euler39 120 `shouldBe` [120]
    it "returns solution" $
      euler39 1000 `shouldBe` [840]

  describe "euler40" $ do
    it "returns example" $
      euler40 [12] `shouldBe` [1]
    it "returns solution" $
      euler40 [1, 10, 100, 1000, 10000, 100000, 1000000] `shouldBe` [210]

  describe "euler41" $ do
    it "returns solution" $
      euler41 `shouldBe` [7652413]

  describe "euler42" $ do
    it "returns example" $ do
      euler42 "SKY" `shouldBe` [1]
    it "returns solution" $ do
      words <- readFile "words.txt"
      euler42 words `shouldBe` [162]

  describe "euler43" $ do
    xit "returns solution" $
      euler43 `shouldBe` [16695334890]

  describe "euler44" $ do
    it "returns solution" $
      euler44 `shouldBe` [(7042750, 1560090)]

  describe "euler45" $ do
    it "returns solution" $
      euler45 `shouldBe` [40755, 1533776805]

  describe "euler46" $ do
    it "returns solution" $
      euler46 `shouldBe` [5777]

  describe "euler47" $ do
    it "returns example" $
      euler47 600 3 `shouldBe` [644, 645, 646]
    it "returns solution" $
      euler47 134040 4 `shouldBe` [134043, 134044, 134045, 134046]

  describe "euler48" $ do
    it "returns example" $
      euler48 10 `shouldBe` 10405071317
    it "returns solution" $
      euler48 1000 `shouldBe` 1000368199144695177095375011227646795567793680622934654583760988100234910747716194381428659099527845945869942643191290894720342979906407679647259860434238468038326040809691037615370376237713648510063115732951461774246705584266865759601815843666442832284556880313114548151539190975398485496645576513465858582712336401166221956188173449531674102688908321764663020306699770408625340766091595022791379368098369306375602813856646358773751558775213460225796579846583334007349358624342339332981334571237888809283103348760261360175950815609179464026871005243652109980863552142014242903434068560936573231079342194031864413918101238151056509267393515760392842472501391594073463001521843811073767021711026307504695733467897821866906648469828346607412967395801797791683609834722432241952845352564681868240369569566192825555323558078061997527689983848863374786789331581565252059172614339424600986143259233167583371070362625554531852054166117148858229508581589614337594463277554380518380921301218836327102231407332201109740102580216469298331766920619646083790732807627360614428085171565006289728508688964226799647192582924058589530750674578385365561878559589685756225692348914746922810913915619834754117648358035814128670294158565669942087736286390942241547226015004471330630113072042704288905042142628193771918594574302202147201188486345913190833752307476966010547423928871063118783026036381319039052008252072057933666712918946233312793697094074224187872045970976444309242782187738320257490080824330074991698698239561125811127607863900355221737846690567707344074494145266662103839812840216303448476913957072355732716627098372245223046792919747259113157425824064858331415400943278213042954635053574045209984512221264241903550178416824551412548637590007779082539288247751653566899882749594405895102587985539527709493510049546445427265617478399107188238681771215904234119392247489751079085948055945098805617963722928469554263782217625160428008228845552540344494860195267115187092227766195753907211126646150140614744233974765273475619964311852858614167819668340124730487710162006793529985758820653677274379563313495454526632718723482339494825759821076401694316043456512117937935456463521463021197726694983558929132357576188594977516630734212863869456164205525536767311298137182511494649463663073759219213056823561667776093739425742883930712609962163464088038826569132032160692637206183085942987973684584276491784843115472077900401692595694119273553511025991265446039366288921743581333200083717105241171504606883543418862024047552177055263424469501298905901938158245938633694105024815166679813689156668341197713475094389904887126794468901893850475050011205225742455555625750560213230387910337983950333245020653238989115507013882956277763880795687210857196493893142656713105966275422144605988058939600603604226921401402096519294250488670297983396353279460453142375542267881989197481789780678955093763193658603690898474826976906544473978017455720367929981796023041785852626797271283465789498383642350667978127819110846700

  describe "euler49" $ do
    it "returns solution" $
      euler49 `shouldBe` [[1487, 4817, 8147], [2969, 6299, 9629]]

  describe "euler50" $ do
    it "returns example" $
      euler50 100 `shouldBe` [(41, 6)]
    it "returns example" $
      euler50 1000 `shouldBe` [(953, 21)]
    xit "returns solution" $
      euler50 1000000 `shouldBe` [(997651, 543)]

  describe "euler52" $ do
    it "returns solution" $
      euler52 `shouldBe` [142857]

  describe "euler53" $ do
    it "returns example" $
      euler53 [1 .. 23] `shouldBe` [4]
    it "returns solution" $
      euler53 [1 .. 100] `shouldBe` [4075]

  describe "euler55" $ do
    it "returns example" $
      euler55 [47, 349, 196, 4664] `shouldBe` [1]
    xit "returns solution" $
      euler55 [0 .. 9999] `shouldBe` [249]

  describe "euler56" $ do
    it "returns solution" $
      euler56 [90 .. 100] [90 .. 100] `shouldBe` [972]

  describe "euler57" $ do
    it "returns solution" $
      euler57 [1 .. 8] `shouldBe` [1]
    it "returns solution" $
      euler57 [1 .. 1000] `shouldBe` [153]

  describe "euler58" $ do
    xit "returns solution" $
      euler58 `shouldBe` [26241]

  describe "euler59" $ do
    xit "returns solution" $ do
      cipher <- readFile "p059_cipher.txt"
      euler59 cipher `shouldBe` [129448]
