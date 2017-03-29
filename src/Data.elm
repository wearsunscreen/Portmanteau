module Data exposing (getDefinition, getHint, getWord)

import List exposing (drop, head, take)
import List.Extra exposing ((!!), getAt)
import Maybe exposing (withDefault)
import Model exposing ((??))
import String exposing (split)


{-|
    Get the definition from the source data.
    If this returns "nothing, then probably the source data is malformed
-}
getDefinition : Int -> Maybe String
getDefinition n =
    (rawData !! n)
        ?? "x:invalid-index"
        |> split ":"
        |> getAt 1


{-| Get source words of the portmanteau
-}
getHint : Int -> Int -> Maybe String
getHint n hint =
    (rawData !! n)
        ?? "x:invalid-index"
        |> split "$"
        |> getAt 1


{-| get the portmanteau word
-}
getWord : Int -> Maybe String
getWord n =
    head <| split " " <| (rawData !! n) ?? "invalid-index"


rawData =
    [ "administrivia $administrative/trivia$:dull administration activities that must be completed."
    , "affluenza $affluence/influenza$:the guilt or lack of motivation experienced by people who have made or inherited large amounts of money."
    , "anacronym $anachronism/acronym$:an acronym that is derived from a phrase that is no longer in wide usage (for example, radar)."
    , "animatronic $animate/electronics$:robots that are constructed to look like animals."
    , "anticipointment $anticipation/disappointment$:the feeling of letdown one experiences when hype gives way to reality. "
    , "bionic $biology/electronic$:artificial body parts that have been enhanced by technology. "
    , "bodacious $bold/audacious$:insolent or unrestrained, extraordinary or impressively large. "
    , "Bollywood $Bombay/Hollywood$:the Indian movie industry. "
    , "Brangelina $Brad/Angelina$:word used to describe the celebrity couple Brad Pitt and Angelina Jolie. "
    , "bromance $brother/romance$:a close relationship between two men. "
    , "brunch $breakfast/lunch$:a meal that is eaten after breakfast but before lunch. "
    , "carjack $car/hijack$:to take someone’s car by force. "
    , "chillax $chill/relax$:calm down, rest. "
    , "Chinglish $Chinese/English$:a variation of the English language as spoken by people of Chinese descent. "
    , "chocoholic $chocolate/alcoholic$:someone who eats excessive amounts of chocolate. "
    , "chortle $chuckle/snort$:laugh in a breathy, gleeful way. "
    , "Chunnel $channel/tunnel$:word used to describe the Channel Tunnel that runs between the UK and France. "
    , "cineplex $cinema/complex$:a movie theatre with several screens. "
    , "Cocacolonization $Coca-Cola/colonization$:the aggressive introduction or pervasive influence of American consumerism on other cultures. "
    , "cosplay $costume/play$:dressing up in costumes that resemble characters from popular culture. "
    , "craptacular $crap/spectacular$:entertainment that is so poor in quality it captivating. "
    , "crunk $crazy/drunk$:out of control after consuming alcohol. "
    , "cyborg $cybernetic/organism$:A human or fictional entity whose physiological functioning is enhanced by mechanical elements. "
    , "docusoap $documentary/soap opera$:a hybrid drama that follows the lives of real people over a given period of time. "
    , "dumbfound $dumb/confound$:Greatly astonish or amaze. "
    , "ebonics $ebony/phonics$:A distinct form of English that is spoken by people of African descent. "
    , "edutainment $education/entertainment$:Games or other forms of entertainment that have an educational aspect. "
    , "electrocution $electricity/execution$:Death by electricity. "
    , "emoticon $emotion/icon$:The use of keyboard characters to represent a facial expression. "
    , "evailable $electronic/available$:to be available online via an electronic method such as email. "
    , "faction $fact/fiction$:a story, speech or novel that contains historical and real-life facts combined with invented information. "
    , "fanzine $fan/magazine$:a magazine that is targeted at fans of a specific genre. "
    , "feminazi $feminist/Nazi$:An extreme feminist. "
    , "flare $flame/glare$:a sudden brief burst of bright flame or light. "
    , "flexitarian $vegetarian/flexible$:A vegetarian who occasionally eats meat. "
    , "frankenfood $Frankenstein/food$:genetically modified food. "
    , "frenemy $friend/enemy$:someone who is supposed to be a friend but whose actions are more characteristic of a foe. "
    , "gaydar $gay/radar$:the ability to identify whether a person is homosexual based on an observation of their appearance and/or behavior. "
    , "geocaching $geography/caching$:a modern-day treasure hunt in which participants use a GPS to hide and seek containers. "
    , "ginormous $giant/enormous$:large, huge. "
    , "glamping $glamour/camping$:luxury camping. "
    , "glitz $glamour/Ritz$:extravagant yet superficial. "
    , "guesstimate $guess/estimate$:to estimate without solid facts or figures. "
    , "infomercial $information/commercial$:A television program that promotes a product in an informative and supposedly objective way. "
    , "infotainment $information/entertainment$:forms of popular media that blend information and entertainment together. Similar to edutainment $education/entertainment). "
    , "interrobang $interrogative/bang$:a combination of a question mark and an exclamation point. "
    , "irregardless $irrespective/regardless). "
    , "internet $international/network$:a global system of interconnected computer networks. You can access our proofreading service via the Internet. "
    , "interrobang $interrogative/bang$:a combination of a question mark and an exclamation point. "
    , "jackalope $jackrabbit/antelope$:An antlered species of rabbit (mythical). "
    , "knowledgebase $knowledge/database$:intellectual capital that is stored in a central area. Similar to wisdombase (wisdom/database). "
    , "liger $lion/tiger$:a crossbreed between a lion and a tiger. "
    , "malware $malicious/software$:computer programs that are designed to damage or disable computer systems. "
    , "mansplaining $man/explaining$:Explaining something to a woman in a condescending manner. "
    , "manwich $man/sandwich$:A sandwich made from any of the ingredients that are available in the fridge. "
    , "McMansion $McDonalds/mansion$:a blandly generic large house. "
    , "meld $melt/weld$:Blend/combine. "
    , "metrosexual $metropolitan/heterosexual$:a man who appears to be inordinately concerned about personal aesthetics and/or is perceived for this quality as being homosexual. "
    , "mockumentary $mock/documentary$:A genre of film and television in which fictitious events are presented in documentary-style manner. "
    , "modem $modulation/demodulation$:an electronic device that makes possible the transmission of data to or from a computer via telephone or other communication lines. "
    , "motel $motor/hotel$:overnight accommodation designed for motorists. "
    , "motorcade $motor/cavalcade$:A procession of motor vehicles. "
    , "murse $man/purse$:A man’s purse. "
    , "netizen $internet/citizen$:an individual who is heavily involved with online activities. "
    , "Nintendinitis $Nintendo/tendonitis$:A condition caused by playing too many video games. "
    , "Oxbridge $Oxford/Cambridge$:an inclusive term that is used to describe both Oxford and Cambridge universities. "
    , "pregnesia: $pregnancy/amnesia$:The loss of your short-term memory as a result of pregnancy. "
    , "ridonkulous $ridiculous/donkey$:The epitome of ridiculous. "
    , "rockabilly $rock’n’roll/hill-billy$:A type of popular music, originating in the southeastern US in the 1950s, combining elements of rock and roll and country music. "
    , "skort $skirt/shorts$:A pair of shorts that resemble a skirt. "
    , "screenager $screen/teenager$:the typical adolescent who indulges excessively in screen entertainment. "
    , "scuzz $scum/fuzz$:Something that is regarded as disgusting. "
    , "shemale $she/male/female$:A man disguised as a women. "
    , "shopaholic $shop/alcoholic$:An individual who is addicted to shopping and buying products. "
    , "smog $smoke/fog$:a form of air pollution that has the qualities of both smoke and fog. "
    , "Spanglish $Spanish/English$:A hybrid language that combines English and Spanish. "
    , "spork $spook/fork$:A hybrid form of cutlery. "
    , "stagflation $stagnation/inflation$:Persistent high inflation and unemployment accompanied by stagnant demand. "
    , "telethon $television/marathon$:A very long television program. "
    , "televangelist $television/evangelist$:An evangelist who regularly appears on television. "
    , "three-peat $three/repeat$:A third consecutive victory. "
    , "tomacco $tomato/tobacco$:A hybrid created by grafting a tomato plant onto the roots of a tobacco plant. "
    , "turducken $turkey/duck/chicken$:A dish consisting of a de-boned chicken stuffed into a de-boned duck, which itself is stuffed into a de-boned turkey. "
    , "vidiot $video/idiot$:A habitual, undiscriminating watcher of television or videotapes. "
    , "workaholic $work/alcoholic$:an individual who works excessive hours. "
    ]
