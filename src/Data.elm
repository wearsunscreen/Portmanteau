module Data exposing (getDefinition, getHint, getNumWords, getWord)

import List exposing (drop, head, length, take)
import List.Extra exposing ((!!), getAt)
import Maybe exposing (withDefault)
import Model exposing ((??))
import Result exposing (Result)
import String exposing (repeat, split)


malformedErrorMessage : Int -> String
malformedErrorMessage n =
    "error: malformed data at index " ++ (toString n)


{-|
    Get the definition from the source data.
    If this returns "nothing, then probably the source data is malformed
-}
getDefinition : Int -> String
getDefinition n =
    case getLine n of
        Err msg ->
            msg

        Ok s ->
            split ":" s
                |> getAt 1
                |> withDefault (malformedErrorMessage n)


{-| Get source words of the portmanteau
-}
getHint : Int -> Int -> String
getHint n hint =
    case getLine n of
        Err msg ->
            msg

        Ok s ->
            split "$" s
                |> getAt 1
                |> withDefault ((malformedErrorMessage 2) ++ "/" ++ (malformedErrorMessage n))
                |> split "/"
                |> getAt (hint)
                |> withDefault (malformedErrorMessage n)


{-| local function to get a single line
-}
getLine : Int -> Result String String
getLine n =
    case (rawData !! n) of
        Nothing ->
            Err ("error:invalid-index of " ++ (toString n))

        Just x ->
            Ok x


{-| get the portmanteau word
-}
getWord : Int -> String
getWord n =
    case getLine n of
        Err msg ->
            msg

        Ok s ->
            split "$" s
                |> getAt 0
                |> withDefault ("error: malformed data at index " ++ (toString n))


getNumWords : Int
getNumWords =
    length rawData


rawData =
    [ "turducken $turkey/duck/chicken$:A dish consisting of a de-boned chicken stuffed into a de-boned duck, which itself is stuffed into a de-boned turkey"
    , "workaholic $work/alcoholic$:an individual who works excessive hours"
    , "affluenza $affluence/influenza$:the guilt or lack of motivation experienced by people who have made or inherited large amounts of money"
    , "anacronym $anachronism/acronym$:an acronym that is derived from a phrase that is no longer in wide usage (for example, radar)"
    , "animatronic $animate/electronics$:robots that are constructed to look like animals"
    , "spork $spook/fork$:A hybrid form of cutlery"
    , "stagflation $stagnation/inflation$:Persistent high inflation and unemployment accompanied by stagnant demand"
    , "telethon $television/marathon$:A very long television program"
    , "televangelist $television/evangelist$:An evangelist who regularly appears on television"
    , "three-peat $three/repeat$:A third consecutive victory"
    , "bionic $biology/electronic$:artificial body parts that have been enhanced by technology"
    , "bodacious $bold/audacious$:insolent or unrestrained, extraordinary or impressively large"
    , "Bollywood $Bombay/Hollywood$:the Indian movie industry"
    , "meld $melt/weld$:Blend/combine"
    , "bromance $brother/romance$:a close relationship between two men"
    , "brunch $breakfast/lunch$:a meal that is eaten mid-morning "
    , "carjack $car/hijack$:to take someone’s car by force"
    , "chillax $chill/relax$:calm down, rest"
    , "motorcade $motor/cavalcade$:A procession of motor vehicles"
    , "murse $man/purse$:A man’s purse"
    , "netizen $internet/citizen$:an individual who is heavily involved with online activities"
    , "pregnesia: $pregnancy/amnesia$:The loss of your short-term memory as a result of pregnancy"
    , "chocoholic $chocolate/alcoholic$:someone who eats excessive amounts of chocolate"
    , "chortle $chuckle/snort$:laugh in a breathy, gleeful way"
    , "Chunnel $channel/tunnel$: a route between between the UK and France"
    , "internet $international/network$:a global system of interconnected computer networks. You can access our proofreading service via the Internet"
    , "jackalope $jackrabbit/antelope$:An antlered species of rabbit (mythical)"
    , "knowledgebase $knowledge/database$:intellectual capital that is stored in a central area. Similar to wisdombase (wisdom/database)"
    , "malware $malicious/software$:computer programs that are designed to damage or disable computer systems"
    , "cineplex $cinema/complex$:a movie theatre with several screens"
    , "cosplay $costume/play$:dressing up in costumes that resemble characters from popular culture"
    , "craptacular $crap/spectacular$:entertainment that is so poor in quality it captivating"
    , "cyborg $cybernetic/organism$:A human or fictional entity whose physiological functioning is enhanced by mechanical elements"
    , "docusoap $documentary/soap opera$:a hybrid drama that follows the lives of real people over a given period of time"
    , "dumbfound $dumb/confound$:Greatly astonish or amaze"
    , "vidiot $video/idiot$:A habitual, undiscriminating watcher of television or videotapes"
    , "edutainment $education/entertainment$:Games or other forms of entertainment that have an educational aspect"
    , "electrocution $electricity/execution$:Death by electricity"
    , "emoticon $emotion/icon$:The use of keyboard characters to represent a facial expression"
    , "fanzine $fan/magazine$:a magazine that is targeted at fans of a specific genre"
    , "flare $flame/glare$:a sudden brief burst of bright flame or light"
    , "flexitarian $vegetarian/flexible$:A vegetarian who occasionally eats meat"
    , "frankenfood $Frankenstein/food$:genetically modified food"
    , "frenemy $friend/enemy$:someone who is supposed to be a friend but whose actions are more characteristic of a foe"
    , "gaydar $gay/radar$:the ability to identify whether a person is homosexual based on an observation of their appearance and/or behavior"
    , "geocaching $geography/caching$:a modern-day treasure hunt in which participants use a GPS to hide and seek containers"
    , "ginormous $giant/enormous$:large, huge"
    , "glamping $glamour/camping$:luxury camping"
    , "guesstimate $guess/estimate$:to estimate without solid facts or figures"
    , "infomercial $information/commercial$:A television program that promotes a product in an informative and supposedly objective way"
    , "infotainment $information/entertainment$:forms of popular media that blend information and entertainment together. Similar to edutainment $education/entertainment)"
    , "interrobang $interrogative/bang$:a combination of a question mark and an exclamation point"
    , "mansplaining $man/explaining$:Explaining something to a woman in a condescending manner"
    , "administrivia $administrative/trivia$:dull administration activities that must be completed"
    , "manwich $man/sandwich$:A sandwich made from any of the ingredients that are available in the fridge"
    , "McMansion $McDonalds/mansion$:a blandly generic large house"
    , "metrosexual $metropolitan/heterosexual$:a man who appears to be inordinately concerned about personal aesthetics and/or is perceived for this quality as being homosexual"
    , "mockumentary $mock/documentary$:A genre of film and television in which fictitious events are presented in documentary-style manner"
    , "modem $modulation/demodulation$:an electronic device that makes possible the transmission of data to or from a computer via telephone or other communication lines"
    , "motel $motor/hotel$:overnight accommodation designed for motorists"
    , "crunk $crazy/drunk$:out of control after consuming alcohol"
    , "rockabilly $rock’n’roll/hill-billy$:A type of popular music, originating in the southeastern US in the 1950s, combining elements of rock and roll and country music"
    , "skort $skirt/shorts$:A pair of shorts that resemble a skirt"
    , "screenager $screen/teenager$:the typical adolescent who indulges excessively in screen entertainment"
    , "scuzz $scum/fuzz$:Something that is regarded as disgusting"
    , "shopaholic $shop/alcoholic$:An individual who is addicted to shopping and buying products"
    , "smog $smoke/fog$:a form of air pollution that has the qualities of both smoke and fog"
    , "tomacco $tomato/tobacco$:A hybrid created by grafting a tomato plant onto the roots of a tobacco plant"
    ]



{-
   email = electronic + mail
   alphanumeric = alphabetic + numberic
   phablet = phone + tablet
   podcast = iPod + broadcast
   freeware = free + software
   malware = malicious + software
   webisode = web + episode
   Wifi = wireless + fidelity
   webinar = web + seminar
   netiquette = Internet + etiquette
   wikipedia = wiki + encyclopedia
   Yelp = Yellow pages + help
   shopaholic = shop + alcoholic
   sitcom = situational + comedy
   clasp = clutch + grasp
   splatter = splash + spatter
   electrocute = electric + execute
   biopic = biography + picture
   avionics = aviation + electronics
   Velcro = velvet + crochet (small hook in french)
   metrosexual = metropolitan + heterosexual
   cyborg = cybernetic + organism
   Spam = spiced + ham
   snark = snide + remark
   taxicab = taximeter (tax) + cabriolet (carriage)
   Gerrymandering (Gerry + Salamander)
   HazMat - harzardous
   listicle - List/Article
   sexting - sex/texting
   twerk - twist/jerk
   pixel $picture/elements
   endorphin - endogene/morphine
   Tanzania - Tanganyika/Zanzibar
   moped - motor/pedal
   napalm - naphthenic/palmitic
   modem - modulator/demodulator
   bash - bang/smash
   hassle - haggle/tussle
   pennant - pennon/pendant

-}
