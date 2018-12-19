module Data exposing (getDefinition, getHint, getNumWords, getWord, nextQuestion)

import List exposing (drop, head, length, take)
import List.Extra exposing (getAt)
import Maybe exposing (withDefault)
import Model exposing (justOrDefault)
import Result exposing (Result)
import String exposing (fromInt, repeat, split, trim)


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
                |> trim


{-| local function to get a single line
-}
getLine : Int -> Result String String
getLine n =
    case (getAt n rawData) of
        Nothing ->
            Err ("error:invalid-index of " ++ (fromInt n))

        Just x ->
            Ok x


getNumWords : Int
getNumWords =
    length rawData


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
                |> withDefault ("error: malformed data at index " ++ (fromInt n))
                |> trim


malformedErrorMessage : Int -> String
malformedErrorMessage n =
    "error: malformed data at index " ++ (fromInt n)


nextQuestion : Int -> Int
nextQuestion n =
    modBy (n + 1) (length rawData)
    

rawData =
    [ "telethon $television/marathon$:A very long television program, often to raise money"
    , "workaholic $work/alcoholic$:an individual who works excessive hours"
    , "email $electronic/mail$:correspondence over TCP/IP"
    , "wifi $wireless/fidelity$:radio protocol for local computer networks"
    , "sitcom $situational/comedy$:television genre first developed by Desi Arnaz and Lucille Ball"
    , "splatter $splash/spatter$:a spot or trail of a sticky or viscous liquid splashed over a surface or object"
    , "pixel $picture/element$:a minute discreet area of illumination on a display screen"
    , "craptacular $crap/spectacular$:entertainment that is so poor in quality it captivating"
    , "meld $melt/weld$:Blend/combine"
    , "bromance $brother/romance$:a close relationship between two men"
    , "three-peat $three/repeat$:A third consecutive championship"
    , "bodacious $bold/audacious$:insolent or unrestrained, extraordinary or impressively large"
    , "Gerrymandering $Governor Elbridge Gerry/salamander$:manipulate the boundaries of an electoral constituency so as to favor one party"
    , "clasp $clutch/grasp$:a device with interlocking parts used for fastening things together"
    , "pennant $pennon/pendant$:a tapering flag, often denoting a sports championship"
    , "electrocute $electric/execute$:terminate life by applying high voltage"
    , "spork $spook/fork$:A hybrid form of cutlery"
    , "stagflation $stagnation/inflation$:A macro-economic state of rising prices by stagnant demand"
    , "turducken $turkey/duck/chicken$:A dish consisting of a de-boned chicken stuffed into a de-boned duck, which itself is stuffed into a de-boned turkey"
    , "bash $bang/smash$:strike hard and usually noisily"
    , "endorphin $endogene/morphine$:hormones secreted within the brain and nervous system and having a number of physiological functions"
    , "Tanzania $Tanganyika/Zanzibar$:Formed by the merger of two East African nations in 1964, this country contains Mount Kilimanjaro"
    , "podcast $iPod/broadcast$:audio programs delivered over the internet"
    , "malware $malicious/software$:software that is intended to damage or disable computers and computer systems"
    , "alphanumeric $alphabetic/numberic$:a through z, A through Z and 0-9"
    , "moped $motor/pedal$:a early form of motorized bicycle"
    , "webisode $web/episode$:a television program made to be distributed over the internet"
    , "wifi $wireless/fidelity$:radio protocol for local computer networks"
    , "animatronic $animate/electronics$:robots that are constructed to look like animals"
    , "phablet $phone/tablet$:a device that's larger than a phone, smaller than a tablet"
    , "cyborg $cybernetic/organism$:a person whose physical abilities are extended beyond normal human limitations by mechanical elements built into the body"
    , "Spam $spiced/ham$:a canned pre-cooked pork produt introduced by Hormel in 1937"
    , "taxicab $taximeter/cabriolet$:a car licensed to transport passengers in return for payment of a fare"
    , "webinar $web/seminar$:a training or demonstration session viewed over the internet"
    , "netiquette $Internet/etiquette$:the correct or acceptable way of communicating on the Internet"
    , "napalm $naphthenic/palmitic$:a mixture of a gelling agent and gasoline, it \"smells like ... victory\""
    , "modem $modulator/demodulator$:a device used to connect digital networks to analog phone lines"
    , "twerk $twist/jerk$:dancing in a sexually provocative manner involving thrusting hip movements and a low, squatting stance"
    , "avionics $aviation/electronics$:electronic equipment fitted in an aircraft"
    , "Velcro $velvet/crochet$:a fastener for clothes or other items, consisting of two strips, one covered with tiny loops and the other with tiny flexible hooks"
    , "metrosexual $metropolitan/heterosexual$:a young, urban, heterosexual male with liberal political views, an interest in fashion, and meticulous about his grooming and appearance"
    , "hazmat $harzardous/material$:a flammable, explosive or poisonous material"
    , "listicle $List/Article$:a piece of writing or other content presented wholly or partly in the form of a list"
    , "sexting $sex/texting$:sending sexually explicit photographs or messages via mobile phone."
    , "televangelist $television/evangelist$:A preacher who regularly appears on television"
    , "bionic $biology/electronic$:artificial body parts that have been enhanced by technology"
    , "Bollywood $Bombay/Hollywood$:the Indian movie industry"
    , "chillax $chill/relax$:calm down, take it easy"
    , "affluenza $affluence/influenza$:the guilt or lack of motivation experienced by people who have made or inherited large amounts of money"
    , "rockabilly $rock’n’roll/hill-billy$:A type of popular music, originating in the southeastern US in the 1950s, combining elements of rock and roll and country music"
    , "glamping $glamour/camping$:luxury camping"
    , "knowledgebase $knowledge/database$:intellectual capital that is stored in a central area. Similar to wisdombase (wisdom/database)"
    , "dumbfound $dumb/confound$:Greatly astonish or amaze"
    , "cineplex $cinema/complex$:a movie theatre with several screens"
    , "cosplay $costume/play$:dressing up in costumes that resemble characters from popular culture"
    , "frenemy $friend/enemy$:someone who is supposed to be a friend but whose actions are more characteristic of a foe"
    , "gaydar $gay/radar$:the ability to identify whether a person is homosexual based on an observation of their appearance and/or behavior"
    , "anacronym $anachronism/acronym$:an acronym that is derived from a phrase that is no longer in wide usage (for example, radar)"
    , "shopaholic $shop/alcoholic$:Someone addicted to shopping"
    , "skort $skirt/shorts$:A pair of shorts that resemble a skirt"
    , "biopic $biography/picture$:a movie depicted the life of a person"
    , "brunch $breakfast/lunch$:a meal that is eaten mid-morning "
    , "carjack $car/hijack$:to take someone’s car by force"
    , "motorcade $motor/cavalcade$:A procession of motor vehicles"
    , "murse $man/purse$:A man’s purse"
    , "netizen $internet/citizen$:an individual who is heavily involved with online activities"
    , "pregnesia $pregnancy/amnesia$:The loss of your short-term memory as a result of pregnancy"
    , "chortle $chuckle/snort$:laugh in a breathy, gleeful way"
    , "Chunnel $channel/tunnel$: a route between between the UK and France"
    , "jackalope $jackrabbit/antelope$:An antlered species of rabbit (mythical)"
    , "cyborg $cybernetic/organism$:A human or fictional entity whose physiological functioning is enhanced by mechanical elements"
    , "vidiot $video/idiot$:A habitual, undiscriminating watcher of television or videotapes"
    , "edutainment $education/entertainment$:Games or other forms of entertainment that have an educational aspect"
    , "emoticon $emotion/icon$:The use of keyboard characters to represent a facial expression"
    , "fanzine $fan/magazine$:a magazine that is targeted at fans of a specific genre"
    , "ginormous $giant/enormous$:large, huge"
    , "guesstimate $guess/estimate$:to estimate without solid facts or figures"
    , "infomercial $information/commercial$:A television program that promotes a product in an informative and supposedly objective way"
    , "malware $malicious/software$:computer programs that are designed to damage or disable computer systems"
    , "interrobang $interrogative/bang$:a combination of a question mark and an exclamation point"
    , "McMansion $McDonalds/mansion$:a blandly generic large house"
    , "metrosexual $metropolitan/heterosexual$:a man who appears to be inordinately concerned about personal aesthetics and/or is perceived for this quality as being homosexual"
    , "mockumentary $mock/documentary$:A genre of film and television in which fictitious events are presented in documentary-style manner"
    , "motel $motor/hotel$:overnight accommodation designed for motorists"
    , "crunk $crazy/drunk$:out of control after consuming alcohol"
    , "frankenfood $Frankenstein/food$:genetically modified food"
    , "geocaching $geography/caching$:a modern-day treasure hunt in which participants use a GPS to hide and seek containers"
    , "smog $smoke/fog$:a form of air pollution that has the qualities of both smoke and fog"
    ]
