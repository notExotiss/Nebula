#Requires AutoHotkey v2.0
#SingleInstance Force
SendMode "Input"
SetWorkingDir A_ScriptDir
CoordMode "Mouse", "Window"
CoordMode "Pixel", "Window"

; --- Include the OCR Library ---
#Include "..\Lib\OCR.ahk"

; --- Helper Functions ---
Rand(min, max) {
    return Random(min, max)
}

; --- Key Typing Function ---
TypeKey(char) {
    global keyHeldLength, keyOverlapProbability, lastKeyShortenFactor
    
    ; Determine if this key should have key overlap (humans sometimes press next key before releasing previous)
    shouldOverlap := Rand(0, 1.0) < keyOverlapProbability
    
    ; Apply dynamic key hold duration - critical for avoiding detection
    actualHoldTime := Round(keyHeldLength * lastKeyShortenFactor)
    
    ; Essential: Vary key durations between small range to avoid statistical detection
    ; Monkeytype checks for coefficient of variation in key durations
    if (Rand(1, 100) <= 65) {
        ; 65% of the time, use normal distribution range
        holdVariance := Rand(-2, 2)  ; Reduced variance (was -3 to 3)
    } else if (Rand(1, 100) <= 85) {
        ; 20% of the time, use wider range (humans have occasional outliers)
        holdVariance := Rand(-5, 5)  ; Reduced variance (was -7 to 7)
    } else {
        ; 15% of the time, use very wide range (crucial for anti-cheat evasion)
        holdVariance := Rand(-8, 8)  ; Reduced variance (was -10 to 10)
    }
    
    ; Apply variance
    actualHoldTime += holdVariance
    
    ; Safety limits - humans have minimum physical constraints
    if (actualHoldTime < 10) actualHoldTime := 10 + Rand(0, 5)  ; More realistic minimum (was 6 + Rand(0, 3))
    if (actualHoldTime > 55) actualHoldTime := 55 - Rand(0, 6)  ; More realistic maximum (was 40 - Rand(0, 4))
    
    ; Very occasionally, simulate a "fat finger" press (longer duration)
    if (Rand(1, 120) <= 1) {  ; More realistic frequency (was 1 in 180)
        actualHoldTime *= 1.25 + (Rand(0, 20) / 100)  ; More realistic multiplier (was 1.15)
    }
    
    ; Check if the character is a standard typing key
    if (char = "a" || char = "b" || char = "c" || char = "d" || char = "e" || 
        char = "f" || char = "g" || char = "h" || char = "i" || char = "j" || 
        char = "k" || char = "l" || char = "m" || char = "n" || char = "o" || 
        char = "p" || char = "q" || char = "r" || char = "s" || char = "t" || 
        char = "u" || char = "v" || char = "w" || char = "x" || char = "y" || 
        char = "z" || char = "'" || char = "," || char = "." || char = " ") {
        
        ; Send the key with keypress/release events to track key overlap
        ; This is critical because Monkeytype's anti-cheat measures keyOverlap stats
        if (shouldOverlap && Rand(1, 100) <= 68) {  ; More realistic frequency (was 78)
            ; Simulate key overlap (next key pressed before current is released)
            ; This is a natural human typing pattern and critical for avoiding detection
            SendInput "{" char " down}"
            Sleep actualHoldTime * 0.7  ; More realistic hold time (was 0.6)
            SendInput "{" char " up}"
        } else {
            ; Normal key press with variable timing
            SendInput "{" char " down}"
            Sleep actualHoldTime
            SendInput "{" char " up}"
        }
    }
    ; Handle special characters
    else if (char = "!")
        SendInput "!"
    else if (char = "@") 
        SendInput "@"
    else if (char = "#") 
        SendInput "#"
    else if (char = "$") 
        SendInput "$"
    else if (char = "%") 
        SendInput "%"
    else if (char = "^") 
        SendInput "^"
    else if (char = "&") 
        SendInput "&"
    else if (char = "*") 
        SendInput "*"
    else if (char = "(") 
        SendInput "("
    else if (char = ")") 
        SendInput ")"
    else if (char = "?") 
        SendInput "?"
    else if (char = ":") 
        SendInput ":"
    else if (char = ";") 
        SendInput ";"
    else if (char = "/") 
        SendInput "/"
    else if (char = "<") 
        SendInput "<"
    else if (char = ">") 
        SendInput ">"
    else if (char = "-") 
        SendInput "-"
    else if (char = "=") 
        SendInput "="
    else if (char = "[") 
        SendInput "["
    else if (char = "]") 
        SendInput "]"
    else
        SendInput char
}

; --- User Parameters ---
toggleKey := "^+9"           ; Hotkey: Ctrl+Shift+9 to start typing
targetWPM := 180             ; More realistic target WPM (was 220)
varianceWPM := 18            ; Higher variance for more human-like (was 12)
baseAccuracy := 0.9985       ; More realistic accuracy (was 0.9998)
finalAccuracy := 0.9995      ; More realistic accuracy (was 0.9999)
testDuration := 15           ; Test duration in seconds (was 15)
endSprintFactor := 0.94      ; More natural sprint behavior (was 0.92)
endSprintStart := 24         ; Start sprint later, more natural (was 20)

; --- Common Variables ---
global isTyping := false
global totalCharsTyped := 0
global startTime := 0
global lastCharTime := 0
global lastChar := ""
global currentBurstSpeed := 0
global burstModeActive := false
global consecutiveCorrect := 0
global currentPatternIndex := 1
global patternStartTime := 0
global lastErrorTime := 0           ; Track when the last error occurred
global lastMicroHesitation := 0     ; Last micro hesitation time
global microHesitationCounter := 0  ; Count micro hesitations for more realistic patterns
global lastWordHesitation := 0      ; Track word-level hesitations
global realisticStartDelay := 0     ; Variable for human-like startup delay
global handFatigueAmount := 0       ; Track hand fatigue over time (increases randomly)
global nonUniformVariability := 0   ; Variable that changes on its own without pattern
global realHumanFluctuation := 0    ; Mimics natural human typing fluctuations
global anticipationDelay := 0       ; Delay when longer or complex words are upcoming
global pauseAfterErrorChance := 0   ; Variable chance to pause after error correction
global keyDurationVariance := 0     ; Variance in key press duration
global keyOverlapProbability := 0   ; Probability of key overlap (critical for anticheat)
global lastKeyShortenFactor := 0    ; Factor to shorten last key in a burst

; Initialize arrays properly
global lastFiveKeyTimes := [0, 0, 0, 0, 0]
global keySpacingValues := []
global keyDurationValues := []
global weightedChoices := []

; --- Finger Mapping Model ---
global fingerMap := Map(
    "1qaz", "leftPinky",
    "2wsx", "leftRing", 
    "3edc", "leftMiddle",
    "4rfvb", "leftIndex",
    "5tgb", "leftIndex",
    "6yhn", "rightIndex",
    "7ujm", "rightIndex",
    "8ik,", "rightMiddle",
    "9ol.", "rightRing",
    "0p;/", "rightPinky"
)

; --- Typing Patterns (for rhythm) ---
global typingPatterns := [
    Map("burst", 0.96, "duration", 700),   ; Short burst
    Map("burst", 0.98, "duration", 1800),  ; Moderate rhythm
    Map("burst", 1.03, "duration", 1200),  ; Slight slowdown
    Map("burst", 0.97, "duration", 900),   ; Brief speed improvement
    Map("burst", 1.08, "duration", 2100),  ; Noticeable slowdown
    Map("burst", 0.94, "duration", 1600),  ; Extended speed burst
    Map("burst", 1.02, "duration", 1100),  ; Return to normal
    Map("burst", 0.99, "duration", 1300),  ; Almost normal
    Map("burst", 0.93, "duration", 950)    ; Short fast burst
]

; --- Fast Digraph Speeds (letter pairs) ---
global fastDigraphs := Map(
    "th", 0.68, "he", 0.68, "in", 0.68,
    "er", 0.68, "an", 0.68, "re", 0.68,
    "on", 0.68, "at", 0.68, "en", 0.68,
    "nd", 0.72, "ti", 0.72, "es", 0.72,
    "or", 0.72, "te", 0.72, "of", 0.68,
    "ed", 0.68, "is", 0.68, "it", 0.68,
    "al", 0.72, "ar", 0.72, "st", 0.72,
    "to", 0.68, "nt", 0.72, "ng", 0.72,
    "se", 0.72, "ha", 0.68, "as", 0.68,
    "ou", 0.72, "io", 0.72, "le", 0.72,
    "ve", 0.72, "co", 0.72, "me", 0.68,
    "de", 0.72, "hi", 0.68, "ri", 0.72,
    "ro", 0.72, "ic", 0.72, "ne", 0.68
)

; --- Common Word Speeds ---
global commonWords := Map(
    "the", 0.70, "be", 0.70, "to", 0.70,
    "of", 0.70, "and", 0.70, "a", 0.68,
    "in", 0.70, "that", 0.75, "have", 0.75,
    "i", 0.68, "it", 0.70, "for", 0.75,
    "not", 0.75, "on", 0.70, "with", 0.75,
    "he", 0.70, "as", 0.70, "you", 0.75,
    "do", 0.70, "at", 0.70, "this", 0.75,
    "but", 0.75, "his", 0.75, "by", 0.70,
    "from", 0.75, "they", 0.75, "we", 0.70,
    "say", 0.75, "her", 0.75, "she", 0.70,
    "or", 0.70, "an", 0.70, "will", 0.75,
    "my", 0.70, "one", 0.70, "all", 0.70,
    "would", 0.78, "there", 0.75, "their", 0.78,
    "am", 0.70, "go", 0.70, "how", 0.75,
    "then", 0.75, "more", 0.75, "now", 0.70,
    "about", 0.75, "time", 0.75, "know", 0.75
)

; --- Finger Fatigue Tracking ---
global fingerFatigue := Map(
    "leftPinky", 0,
    "leftRing", 0,
    "leftMiddle", 0,
    "leftIndex", 0,
    "rightIndex", 0,
    "rightMiddle", 0,
    "rightRing", 0,
    "rightPinky", 0,
    "thumb", 0
)

; --- Typist Profile (performance characteristics) ---
global typistProfile := Map(
    "burstThreshold", 6,                ; Higher threshold for entering burst mode
    "burstSpeedBonus", 0.12,            ; Smaller burst speed bonus (more natural)
    "burstDecayRate", 0.0022,           ; Faster decay rate (humans can't maintain bursts as long)
    "recoveryRate", 0.988,              ; Fatigue recovery rate
    "fatigueRate", 0.042,               ; Slightly higher fatigue rate (more human)
    "rhythmSensitivity", 0.06,          ; Higher rhythm sensitivity (more variation)
    "errorRecoveryBoost", 0.96,         ; Error recovery boost
    "handAlternationBonus", 0.91,       ; Hand alternation benefits
    "consistencyFactor", 0.88,          ; Lower consistency (more human)
    "thinkingFactor", 0.04,             ; Higher thinking factor (more cognitive delays)
    "naturalIrregularity", 0.05         ; Higher natural variance (crucial for appearing human)
)

; --- Human Typing Helpers ---
GetFingerForKey(char) {
    global fingerMap
    
    ; Safety check - if empty char is provided, return thumb
    if (char = "" || StrLen(char) = 0)
        return "thumb"
    
    lowerChar := Format("{:L}", char)
    for keys, finger in fingerMap {
        if (InStr(keys, lowerChar))
            return finger
    }
    return "thumb"
}

UpdateFingerFatigue(char) {
    global fingerFatigue, typistProfile
    finger := GetFingerForKey(char)
    
    for key, value in fingerFatigue {
        if (value > 0)
            fingerFatigue[key] := value * typistProfile["recoveryRate"]
    }
    
    fingerFatigue[finger] += typistProfile["fatigueRate"]
}

GetFatigueFactor(char) {
    global fingerFatigue
    finger := GetFingerForKey(char)
    return 1 + (fingerFatigue[finger] * 0.12)
}

IsDigraphFaster(prevChar, currentChar) {
    global fastDigraphs
    digraph := prevChar . currentChar
    digraph := Format("{:L}", digraph)
    if (fastDigraphs.Has(digraph))
        return fastDigraphs[digraph] * 0.85  ; Elite typists have much faster digraph typing
    return 0.92  ; Base digraph speed is faster
}

GetWordFamiliarityFactor(word) {
    global commonWords
    lowerWord := Format("{:L}", word)
    if (commonWords.Has(lowerWord))
        return commonWords[lowerWord] * 0.84  ; Elite typists type common words extremely fast
    return 0.92  ; Even unfamiliar words are typed faster by elite typists
}

IsLeadingPosition(text, position) {
    ; Determine if character is at the start of a word (after space or at beginning)
    if (position = 1)
        return true
    
    prevChar := SubStr(text, position-1, 1)
    return (prevChar = " " || prevChar = "`t" || prevChar = "`n")
}

IsTrailingPosition(text, position) {
    ; Determine if character is at the end of a word (followed by space or at end)
    if (position = StrLen(text))
        return true
        
    nextChar := SubStr(text, position+1, 1)
    return (nextChar = " " || nextChar = "`t" || nextChar = "`n" || nextChar = "," || nextChar = "." || nextChar = "?" || nextChar = "!")
}

GetCurrentWord(text, position) {
    ; Extract the current word from text based on position
    textLen := StrLen(text)
    
    ; Find start of current word
    wordStart := position
    while (wordStart > 1) {
        prevChar := SubStr(text, wordStart-1, 1)
        if (prevChar = " " || prevChar = "`t" || prevChar = "`n")
            break
        wordStart--
    }
    
    ; Find end of current word
    wordEnd := position
    while (wordEnd < textLen) {
        nextChar := SubStr(text, wordEnd+1, 1)
        if (nextChar = " " || nextChar = "`t" || nextChar = "`n" || nextChar = "," || nextChar = "." || nextChar = "?" || nextChar = "!")
            break
        wordEnd++
    }
    
    ; Extract and return the word
    return SubStr(text, wordStart, wordEnd - wordStart + 1)
}

; Frame-by-frame realistic simulation with non-detectable human-like inconsistencies
GetHumanDelay(text, position, currentChar) {  ; Renamed parameter from pos to position for clarity
    global baseSpc, startTime, typingPatterns, targetWPM, lastChar, lastCharTime
    global typistProfile, burstModeActive, currentBurstSpeed, consecutiveCorrect
    global endSprintFactor, endSprintStart, testDuration, patternStartTime, currentPatternIndex
    global lastFiveKeyTimes, humanInconsistencyFactor, keyHeldLength, lastLetterTiming
    global keyPressStagger, lastMicroPause, lastMicroHesitation, microHesitationCounter
    global lastWordHesitation, realisticStartDelay, handFatigueAmount, nonUniformVariability
    global realHumanFluctuation, anticipationDelay, keyDurationVariance, keyOverlapProbability
    global lastKeyShortenFactor, keySpacingVarianceMin, keyDurationVarianceMin, wpmConsistencyNoise
    
    ; Initialize variables
    currentWord := GetCurrentWord(text, position)
    patternModifier := 1.0
    positionModifier := 1.0
    
    ; Calculate milliseconds per character with realistic cognitive overhead
    baseSpc := Rand(11, 20)        ; More human-like range (was 8-15)
    
    ; Faster typing for common characters
    if (currentChar = "e" || currentChar = "t" || currentChar = "a" || currentChar = "o" || 
        currentChar = "i" || currentChar = "n") {
        baseSpc *= 0.65            ; More realistic speed advantage (was 0.60)
    }
    
    ; Track elapsed time for realistic changes in pace
    elapsedTime := A_TickCount - startTime
    elapsedSec := elapsedTime / 1000
    elapsedPct := elapsedSec / testDuration
    
    ; Initialize key duration variance - essential for bypassing anticheat detection
    if (keyDurationVariance = 0) {
        ; Initialize with truly random value to avoid statistical detection
        keyDurationVariance := Rand(5, 15) / 100
    }
    
    ; Initialize key overlap probability - crucial for bypassing timing pattern detection
    if (keyOverlapProbability = 0) {
        ; This simulates key overlap that all human typists exhibit
        keyOverlapProbability := Rand(2, 8) / 100
    }
    
    ; Create unpredictable natural human-like fluctuations (crucial for bot detection)
    ; Real humans show micro-variations that don't follow simple patterns
    if (Rand(1, 80) <= 8 && A_TickCount - lastMicroHesitation > 700) {  ; More frequent fluctuations (was 1 in 100)
        ; Generate truly unpredictable variation (appears random but has psychological basis)
        microHesitationCounter++
        lastMicroHesitation := A_TickCount
        
        ; Create different types of micro-fluctuations that all humans exhibit
        if (Rand(1, 10) <= 3) {
            ; Sudden very subtle speed increase (mental "groove" finding)
            humanInconsistencyFactor := 0.97 + (Rand(-3, 3) / 100)
            
            ; When humans speed up, they also change key duration pattern
            keyDurationVariance := Rand(4, 12) / 100
        } else if (Rand(1, 10) <= 7) {
            ; Slight slowdown (cognitive processing)
            humanInconsistencyFactor := 1.04 + (Rand(-3, 3) / 100)
            
            ; When humans slow down, key duration usually increases
            keyDurationVariance := Rand(9, 18) / 100
        } else {
            ; Return to baseline with slight variation
            humanInconsistencyFactor := 1.0 + (Rand(-4, 4) / 100)
            
            ; Reset key duration to more normal values
            keyDurationVariance := Rand(6, 14) / 100
        }
        
        ; Occasionally change key overlap probability - essential for realistic keystroke patterns
        if (Rand(1, 4) = 1) {
            keyOverlapProbability := Rand(1, 9) / 100
        }
    }
    
    ; Monkeytype anti-cheat specifically checks for WPM consistency patterns
    ; Add intentional noise to WPM to evade detection - CRITICAL
    if (Rand(1, 110) <= 1) {
        wpmNoiseAdjustment := Rand(-12, 12) / 100
        humanInconsistencyFactor *= (1 + wpmNoiseAdjustment)
        
        ; Also adjust key overlap to match typing pattern change
        if (wpmNoiseAdjustment < 0) {
            ; Slowing down - reduce key overlap
            keyOverlapProbability *= 0.85
        } else {
            ; Speeding up - increase key overlap
            keyOverlapProbability *= 1.15
        }
        
        ; Clamp to realistic bounds
        if (keyOverlapProbability < 0.01) 
            keyOverlapProbability := 0.01
        if (keyOverlapProbability > 0.12) 
            keyOverlapProbability := 0.12
    }
    
    ; Add non-uniform variability (changes dynamically without pattern)
    ; Bot detection systems look for "too perfect" rhythms
    if (Rand(1, 85) <= 1) {
        nonUniformVariability := Rand(-8, 8) / 100
        
        ; Also change key duration to mimic human variation
        keyDurationVariance += Rand(-3, 3) / 100
        
        ; Clamp to realistic limits
        if (keyDurationVariance < 0.05) 
            keyDurationVariance := 0.05
        if (keyDurationVariance > 0.25) 
            keyDurationVariance := 0.25
    }
    
    ; Apply non-uniform variability
    humanInconsistencyFactor *= (1 + nonUniformVariability)
    
    ; Add realistic hand fatigue that accumulates very slightly over time
    if (Rand(1, 200) <= 1) {
        handFatigueAmount += Rand(1, 5) / 1000
        
        ; Fatigue also affects key duration - humans press harder when tired
        keyDurationVariance += Rand(1, 3) / 100
    }
    
    ; Apply subtle fatigue - even elite typists show very small slowdown over time
    if (elapsedSec > 8 && handFatigueAmount > 0) {
        baseSpc *= (1 + handFatigueAmount)
    }
    
    ; Apply natural cognitive and muscular inconsistency
    baseSpc *= humanInconsistencyFactor * 0.65  ; More realistic but still fast (was 0.6)
    
    ; Add realistic fluctuations that real human typists show (critical to evade validation)
    if (Rand(1, 100) <= 2) {  ; More frequent (was 1 in 120)
        realHumanFluctuation := (Rand(-10, 10) / 100)  ; More pronounced fluctuation (was -6 to 6)
        baseSpc *= (1 + realHumanFluctuation)
    }
    
    ; Apply subtle key shortenings at the end of words - critical for bypassing anticheat
    if (IsTrailingPosition(text, position)) {
        lastKeyShortenFactor := Rand(60, 85) / 100  ; More shortening (was 70-95)
    } else {
        lastKeyShortenFactor := 0.92  ; Slight global shortening (was 1.0)
    }
    
    ; Update timing variables and patterns without detectable patterns
    
    ; Shorter key hold times for more natural patterns (essential for passing key duration checks)
    if (currentChar = "a" || currentChar = "s" || currentChar = "d" || currentChar = "f" || 
        currentChar = "j" || currentChar = "k" || currentChar = "l" || currentChar = ";") {
        ; Home row keys have shorter hold times (greater confidence)
        keyHeldLength := Rand(12, 22) * (1 + (keyDurationVariance / 2))  ; More human-like (was 8-16)
    } else if (currentChar = "e" || currentChar = "t" || currentChar = "a" || currentChar = "o" || 
               currentChar = "i" || currentChar = "n") {
        ; Common letters have shorter hold times
        keyHeldLength := Rand(10, 18) * (1 + (keyDurationVariance / 2.5))  ; More human-like (was 7-14)
    } else if (InStr("zxqpywbmv", currentChar)) {
        ; Less common letters still slightly slower
        keyHeldLength := Rand(14, 25) * (1 + keyDurationVariance)  ; More human-like (was 10-18)
    } else {
        ; Regular keys - faster overall
        keyHeldLength := Rand(12, 22) * (1 + keyDurationVariance)  ; More human-like (was 8-16)
    }
    
    ; Initialize all delay factors
    wordFactor := 1.0
    fatigueFactor := 1.0
    digraphFactor := 1.0
    handAlternationFactor := 1.0
    rhythmFactor := 1.0
    randomFactor := 1.0
    burstFactor := 1.0
    endSprintModifier := 1.0
    travelFactor := 1.0
    cognitiveFactor := 1.0
    warmupFactor := 1.0

    ; Calculate word-specific factor based on current word
    currentWord := GetCurrentWord(text, position)
    if (currentWord != "") {
        wordFactor := GetWordFamiliarityFactor(currentWord)
    }
    
    ; Calculate final delay with all human factors - unique combination for each person
    finalDelay := baseSpc * patternModifier * positionModifier * wordFactor * 
                  fatigueFactor * digraphFactor * handAlternationFactor * 
                  rhythmFactor * randomFactor * burstFactor * endSprintModifier * 
                  travelFactor * cognitiveFactor * warmupFactor
    
    ; Add rare but natural human hesitations - essential for bypassing bot detection
    if (Rand(1, 350) <= 2) {
        hesitationIntensity := 1.15 + (Rand(0, 15) / 100)
        finalDelay *= hesitationIntensity
        
        ; Extremely rarely, add a tiny micro-pause
        if (Rand(1, 10) = 1) {
            Sleep Rand(2, 8)
        }
    }
    
    ; Natural minimum limits (physical constraints) - crucial for avoiding bot detection
    if (finalDelay < 8) {  ; More realistic minimum delay (was 5)
        ; Still need some minimum delay for human-like appearance
        minDelay := 8 + Rand(0, 5)  ; More realistic minimum range (was 5 + Rand(0, 4))
        finalDelay := minDelay
    }
    
    ; Update tracking variables
    lastChar := currentChar
    lastCharTime := A_TickCount
    UpdateFingerFatigue(currentChar)
    
    return finalDelay
}

; Physical key row determination for realistic travel time
GetKeyRow(char) {
    lowerChar := Format("{:L}", char)
    
    ; Row 1 (number row)
    if (InStr("1234567890-=", lowerChar))
        return 1
    ; Row 2 (QWERTY row)
    else if (InStr("qwertyuiop[]\\", lowerChar))
        return 2
    ; Row 3 (home row)
    else if (InStr("asdfghjkl;'", lowerChar))
        return 3
    ; Row 4 (bottom row)
    else if (InStr("zxcvbnm,./", lowerChar))
        return 4
    ; Space bar row
    else if (lowerChar = " ")
        return 5
    
    ; Default
    return 3
}

; More realistic error model for bot detection avoidance
GenerateHumanError(currentChar, text, position, elapsedTime) {
    global lastChar, fingerFatigue, baseAccuracy, finalAccuracy, testDuration, endSprintStart
    global lastErrorTime, lastCharTime, handFatigueAmount
    
    ; Elite typists maintain extremely high accuracy with natural patterns
    currentAccuracy := baseAccuracy + ((finalAccuracy - baseAccuracy) * (elapsedTime / 15000))
    
    ; Initialize error chance based on accuracy
    errorChance := 1 - currentAccuracy
    
    ; Human typists show distinctive error patterns across test duration
    elapsedSec := elapsedTime / 1000
    elapsedPct := elapsedSec / testDuration
    
    ; Test progression affects error rates naturally
    if (elapsedPct < 0.15) {
        ; Initial adjustment period tends to have slightly more errors
        currentAccuracy -= 0.0025 * (1 - (elapsedPct / 0.15))  ; More errors at start (was 0.0015)
    } else if (elapsedPct > 0.85) {
        ; End of test has distinctive error patterns
        if (elapsedPct > 0.95) {
            ; Very end - either highest concentration or fatigue
            endBehaviorType := Rand(1, 10)
            if (endBehaviorType <= 7) {
                ; Most typists: Ultra-focused, almost no errors
                currentAccuracy += 0.001
            } else {
                ; Some typists: Fatigue/rush errors
                currentAccuracy -= 0.002  ; More pronounced fatigue (was 0.0012)
            }
        } else {
            ; Near end but not final moments - increased concentration
            currentAccuracy += 0.0005
        }
    }
    
    ; End of test concentration improves accuracy with natural progression
    if (elapsedSec >= endSprintStart) {
        sprintProgress := (elapsedSec - endSprintStart) / (testDuration - endSprintStart)
        
        ; Individual sprint behavior affects error patterns
        sprintErrorStyle := Rand(1, 3)
        if (sprintErrorStyle = 1) {
            ; Most common: Steady improvement
            currentAccuracy += (1.0 - currentAccuracy) * sprintProgress * 0.80  ; More improvement (was 0.65)
        } else if (sprintErrorStyle = 2) {
            ; Quick improvement, then plateau
            improvementCurve := Min(1, sprintProgress * 1.8)  ; Faster improvement (was 1.5)
            currentAccuracy += (1.0 - currentAccuracy) * improvementCurve * 0.85  ; More improvement (was 0.7)
        } else {
            ; Gradual improvement
            improvementCurve := sprintProgress * 0.9  ; Faster improvement (was 0.8)
            currentAccuracy += (1.0 - currentAccuracy) * improvementCurve * 0.75  ; More improvement (was 0.6)
        }
    }
    
    ; Humans show fatigue-based errors
    if (handFatigueAmount > 0.005) {
        ; More fatigue slightly increases error chance
        currentAccuracy -= handFatigueAmount * 2.5
    }
    
    ; Humans show pattern-based errors with psychological basis
    contextModifier := 1.0
    
    ; Word position affects errors - strong psychological basis
    if (IsLeadingPosition(text, position)) {
        errorChance *= 0.63  ; Leading characters are less likely to be typed incorrectly
        if (position > 1) {
            ; Add cognitive overhead if this is the start of a non-first word
            errorChance *= 0.92  ; Further reduced (was 0.75)
        }
    } else if (IsTrailingPosition(text, position)) {
        errorChance *= 0.84  ; Ending characters also get some benefit, but less
    }

    ; Word length affects error rates (cognitive psychology principle)
    currentWord := GetCurrentWord(text, position)
    wordLen := StrLen(currentWord)
    characterPosition := position - InStr(text, currentWord) + 1
    
    ; Longer words have position-based error patterns
    if (characterPosition > 1 && characterPosition < StrLen(currentWord) && characterPosition >= 3) {
        ; Middle parts of longer words are more error-prone
        errorChance *= 1.25  ; Increased from 1.15 for more realism
    }
    
    ; Human-like character-specific errors based on biomechanics
    charError := 1.0
    lowerChar := Format("{:L}", currentChar)
    finger := GetFingerForKey(currentChar)
    
    ; Finger-specific error patterns reflect biomechanics
    if (finger = "leftPinky" || finger = "rightPinky") {
        ; Pinky fingers have distinctive error patterns
        charError *= 1.08  ; Higher error rate
        
        ; Position-specific pinky errors
        if (lowerChar = "q" || lowerChar = "p" || lowerChar = "z") {
            charError *= 1.06  ; Extreme positions more error-prone
        }
    }
    else if (finger = "leftRing" || finger = "rightRing") {
        ; Ring fingers have distinctive biomechanics
        charError *= 1.04  ; Higher error rate
    }
    else if (finger = "leftIndex" || finger = "rightIndex") {
        ; Index fingers most precise
        charError *= 0.93  ; Lower error rate
    }
    
    ; Key row affects error rates (biomechanical basis)
    keyRow := GetKeyRow(currentChar)
    if (keyRow = 1) {
        ; Number row has higher error rate
        charError *= 1.12
    } else if (keyRow = 3) {
        ; Home row has lowest error rate
        charError *= 0.82
    } else if (keyRow = 4) {
        ; Bottom row moderate error rate
        charError *= 1.03
    }
    
    ; Home row anchor keys are most accurate (strong biomechanical basis)
    if (lowerChar = "f" || lowerChar = "j") {
        charError *= 0.78  ; Extremely accurate due to physical anchoring
    }
    
    ; Fast typing increases errors but varies by typist
    typingSpeed := A_TickCount - lastCharTime
    if (typingSpeed < 35) {
        speedPenalty := (35 - typingSpeed) / 35
        charError *= 1 + (speedPenalty * 0.08)  ; Smaller penalty for elite typists
    }
    
    ; Fatigue increases errors realistically
    if (fingerFatigue[finger] > 0.30) {
        fatiguePenalty := (fingerFatigue[finger] - 0.30) * 0.12
        charError *= 1 + fatiguePenalty
    }
    
    ; Error recency affects next error likelihood (psychological basis)
    timeSinceLastError := A_TickCount - lastErrorTime
    if (timeSinceLastError < 2500) {
        ; Recent errors reduce likelihood of new errors significantly
        recencyFactor := (2500 - timeSinceLastError) / 2500
        charError *= 0.55 + ((1 - 0.55) * (1 - recencyFactor))
    }
    
    ; Check for difficult combinations with cognitive basis
    if (position > 1) {
        prevChar := SubStr(text, position-1, 1)
        prevPair := Format("{:L}", prevChar . currentChar)
        
        ; Common digraphs are mastered by elite typists - less error-prone
        commonDigraphs := ["th", "he", "an", "re", "er", "in", "on", "at", "nd", "st", "en"]
        for digraph in commonDigraphs {
            if (prevPair = digraph) {
                charError *= 0.80  ; Significant improvement on common patterns
            break
        }
    }
    
        ; Awkward digraphs remain challenging even for elite typists
        awkwardDigraphs := ["qp", "zm", "kx", "vb", "jh", "bm", "wz", "fp", "qo"]
        for digraph in awkwardDigraphs {
            if (prevPair = digraph) {
                charError *= 1.14  ; Still challenging even for experts
                break
            }
        }
        
        ; Repeated letters have distinctive error patterns
        if (prevChar = currentChar) {
            ; Double letters are surprisingly error-prone
            if (lowerChar = "l" || lowerChar = "s" || lowerChar = "e" || lowerChar = "t" || lowerChar = "p") {
                charError *= 1.12  ; Common doubles more error-prone
    } else {
                charError *= 1.08  ; All doubles somewhat error-prone
            }
        }
    }
    
    ; Look ahead for character context (highly realistic)
    if (position < StrLen(text)) {
        nextChar := SubStr(text, position+1, 1)
        
        ; Certain upcoming characters affect accuracy on current character
        if (nextChar = " ") {
            ; End of word is approached differently
            charError *= 0.96  ; Less error-prone before space
        } else {
            ; Check for troublesome upcoming combinations
            currPlusNext := Format("{:L}", currentChar . nextChar)
            troublesomeCombos := ["io", "ui", "ou", "ei", "ie"]
            
            for combo in troublesomeCombos {
                if (currPlusNext = combo) {
                    charError *= 1.08  ; Higher error with troublesome upcoming letter
                    break
                }
            }
        }
    }
    
    ; Adjacent key map for realistic error simulation
    adjacentMap := GetAdjacentKeys()
    
    ; Calculate human-like error chance with sophisticated model
    adjustedErrorChance := (1 - currentAccuracy) * contextModifier * charError
    
    ; Elite typists make fewer random errors
    adjustedErrorChance *= 0.75
    
    ; Minimum and maximum error rates never hit extremes
    if (adjustedErrorChance > 0.05) {
        adjustedErrorChance := 0.05  ; Cap maximum error rate
    }
    ; Determine if error occurs with realistic probability
    errChance := Rand(0, 1.0)
    if (errChance < adjustedErrorChance) {
        ; Determine error type based on realistic human patterns
        errorType := Rand(1, 100)
        
        if (errorType <= 72 && adjacentMap.Has(lowerChar)) {
            ; Type 1: Adjacent key error (common human error)
            ; Select an adjacent key with realistic weighting
            choices := adjacentMap[lowerChar]
            
            ; Adjacent keys have different likelihoods based on position
            weightedChoices := []
            
            ; Analyze the position of adjacent keys for more realistic selection
            for i, adjKey in choices {
                weight := 1.0
                
                ; Keys in same row more likely than keys in different rows
                adjKeyRow := GetKeyRow(adjKey)
                if (adjKeyRow = keyRow) {
                    weight *= 1.2  ; Higher weight for same row
                    
                    ; Directly adjacent is more likely than diagonal
                    if (lowerChar = "f" && adjKey = "g") {
                        weight *= 1.3
                    }
                    if (lowerChar = "g" && adjKey = "f") {
                        weight *= 1.3
                    }
                    if (lowerChar = "j" && adjKey = "k") {
                        weight *= 1.3
                    }
                    if (lowerChar = "k" && adjKey = "j") {
                        weight *= 1.3
                    }
                    if (lowerChar = "f" && adjKey = "d") {
                        weight *= 1.3
                    }
                    if (lowerChar = "d" && adjKey = "f") {
                        weight *= 1.3
                    }
                } else {
                    ; Row distance affects likelihood
                    rowDistance := Abs(adjKeyRow - keyRow)
                    if (rowDistance = 1) {
                        weight *= 0.9
                    }
                    if (rowDistance >= 2) {
                        weight *= 0.7
                    }
                }
                ; Use the weighted value to create a selection pool
                weightedCount := Round(weight * 10)
                Loop weightedCount {
                    weightedChoices.Push(adjKey)
                }
            }
            
            ; Select from the weighted pool for more realistic distribution
            if (weightedChoices.Length > 0) {
                choiceIdx := Rand(1, weightedChoices.Length)
                errorChar := weightedChoices[choiceIdx]
            } else {
                ; Fallback if no weighted choices
                choiceIdx := Rand(1, choices.Length)
                errorChar := choices[choiceIdx]
            }
            
            ; Determine if error is noticed based on cognitive principles
            noticeChance := 0.991  ; Base chance for elite typists
            
            ; Various factors affect notice rate
            if (elapsedPct < 0.1) {
                ; Higher vigilance at beginning
                noticeChance += 0.005
            } else if (elapsedPct > 0.9) {
                ; Attention may vary at end
                if (Rand(1, 10) <= 7) {
                    ; Increased focus for most
                    noticeChance += 0.004
                } else {
                    ; Decreased attention for some
                    noticeChance -= 0.009
                }
            }
            
            ; Error notice chance is also affected by typing speed
            if (typingSpeed < 40) {
                ; Fast typing slightly lowers notice chance
                noticeChance -= 0.004
            }
            
            ; Final determination if error is noticed
            errorNoticed := Rand(0, 1.0) < noticeChance
            
            if (errorNoticed) {
                lastErrorTime := A_TickCount
                return Map(
                    "char", errorChar,
                    "correct", true,
                    "type", "adjacent"
                )
            } else {
                return Map(
                    "char", errorChar,
                    "correct", false,
                    "type", "adjacent"
                )
            }
        }
        else if (errorType <= 87 && position < StrLen(text)) {
            ; Type 2: Transposition error (very common human error)
            nextChar := SubStr(text, position+1, 1)
            
            ; Cognitive basis for error detection
            noticeChance := 0.996  ; Elite typists extremely good at catching these
            
            ; Factors affecting transposition notice
            if (typingSpeed < 35) {
                ; Very fast typing reduces notice chance
                noticeChance -= 0.007
            }
            
            ; Word position affects detection
            if (position > 1 && SubStr(text, position-1, 1) = " ") {
                ; Beginning of word transpositions more likely to be noticed
                noticeChance += 0.003
            }
            
            ; Final determination
            errorNoticed := Rand(0, 1.0) < noticeChance
            
            if (errorNoticed) {
                lastErrorTime := A_TickCount
                return Map(
                    "char", nextChar,
                    "correct", true,
                    "type", "transposition",
                    "extraChar", currentChar
                )
            } else {
                return Map(
                    "char", nextChar,
                    "correct", false,
                    "type", "transposition"
                )
            }
        }
        else if (errorType <= 95) {
            ; Type 3: Double character error (less common but natural)
            ; Cognitive basis for detection
            noticeChance := 0.994  ; Elite typists catch these consistently
            
            ; Extra context - some characters more likely to be doubled
            if (lowerChar = "e" || lowerChar = "o" || lowerChar = "t" || lowerChar = "l") {
                ; Common double-letter characters
                noticeChance -= 0.004  ; Slightly less likely to notice
            }
            
            ; Typing speed affects notice
            if (typingSpeed < 30) {
                noticeChance -= 0.005  ; Harder to notice at very high speeds
            }
            
            ; Final determination
            errorNoticed := Rand(0, 1.0) < noticeChance
            
            if (errorNoticed) {
                lastErrorTime := A_TickCount
                return Map(
                    "char", currentChar,
                    "correct", true,
                    "type", "double",
                    "extraChar", currentChar
                )
            } else {
                return Map(
                    "char", currentChar,
                    "correct", false,
                    "type", "double",
                    "extraChar", currentChar
                )
            }
        }
        else {
            ; Type 4: Omission error (rare but natural)
            ; Cognitive basis for detection
            noticeChance := 0.985  ; Hardest to notice but still high for elite typists
            
            ; Character position affects detection
            if (position > 1 && SubStr(text, position-1, 1) = " ") {
                ; First letter omissions more noticeable
                noticeChance += 0.01
            } else if (position < StrLen(text) && SubStr(text, position+1, 1) = " ") {
                ; Last letter omissions also more noticeable
                noticeChance += 0.005
            }
            
            ; Final determination
            errorNoticed := Rand(0, 1.0) < noticeChance
            
            if (errorNoticed) {
                lastErrorTime := A_TickCount
                return Map(
                    "char", "",
                    "correct", true,
                    "type", "omission"
                )
            } else {
                return Map(
                    "char", "",
                    "correct", false,
                    "type", "omission"
                )
            }
        }
    }
    
    ; No error - return correct character
    return Map(
        "char", currentChar,
        "correct", true,
        "type", "none"
    )
}  ; End of GenerateHumanError function

; Natural human error correction
SimulateRealisticErrorCorrection(errorResult, correctChar) {
    global lastCharTime
    
    ; Human reaction time varies based on error type
    reactionTime := 0
    errorType := errorResult["type"]
    
    if (errorType = "adjacent") {
        ; Adjacent key errors: noticed fairly quickly
        reactionTime := Rand(45, 90)
    }
    else if (errorType = "transposition") {
        ; Transposition errors take slightly longer to process
        reactionTime := Rand(50, 105)
    }
    else if (errorType = "double") {
        ; Double character errors are noticed immediately
        reactionTime := Rand(35, 70)
    }
    else if (errorType = "omission") {
        ; Omission errors take longest to notice
        reactionTime := Rand(60, 120)
    }
    else {
        ; Generic fallback
        reactionTime := Rand(40, 100)
    }
    
    ; Natural human variation in reaction
    if (Rand(1, 10) <= 2) {
        ; Occasionally take longer to react
        reactionTime *= 1.2
    }
    
    Sleep reactionTime
    
    ; Apply different correction patterns for different error types
    if (errorType = "adjacent") {
        ; Standard error correction for adjacent key errors
        SendInput "{Backspace}"
        backspaceDelay := Rand(15, 30)
        Sleep backspaceDelay
        
        TypeKey(correctChar)
        return 2
    }
    else if (errorType = "transposition") {
        ; Transposition errors often require deleting both characters
        SendInput "{Backspace}"
        Sleep Rand(15, 25)
        SendInput "{Backspace}"
        Sleep Rand(20, 35)
        
        ; Type both characters in correct order
        TypeKey(correctChar)
        Sleep Rand(40, 70)
        
        ; Type next character that was moved forward
        if (errorResult.HasOwnProp("extraChar")) {
            TypeKey(errorResult["extraChar"])
            lastCharTime := A_TickCount - Rand(10, 20)  ; Adjust timestamp for flow
            return 4  ; 2 backspaces + 2 characters
        } else {
            return 3  ; Just 2 backspaces + 1 character
        }
    }
    else if (errorType = "double") {
        ; Simply delete extra character
        SendInput "{Backspace}"
        Sleep Rand(15, 25)
        
        ; The correct character is already there, no need to type again
        return 1
    }
    else if (errorType = "omission") {
        ; Just type the missed character, no backspace needed
        TypeKey(correctChar)
        return 1
    }
    else {
        ; Standard correction fallback
        SendInput "{Backspace}"
        Sleep Rand(20, 35)
        
        TypeKey(correctChar)
        return 2
    }
}

; --- OCR Text Extraction Function ---
GetMonkeytypeText() {
    ; Use OCR to get the text from the current window
    try {
        ; Use exact pixel coordinates as specified
        x := 170       ; 170 pixels from the left as requested
        y := 400       ; 400 pixels down from the top
        w := 1550      ; Width adjusted for the new x position
        h := 120       ; Increased height slightly (was 100)
        
        ; Highlight the capture area briefly so the user knows what's being scanned
        WinActivate "A"
        HighlightArea(x, y, w, h)
        
        ; Get OCR result from the exact coordinates with improved scale
        ocrResult := OCR.FromRect(x, y, w, h, {scale: 3})  ; Higher scale for better recognition
        
        ; Extract the text
        fullText := ocrResult.Text
        
        ; Clean up the text
        cleanedText := RegExReplace(fullText, "[^\w\s]", "") ; Remove non-alphanumeric characters
        cleanedText := RegExReplace(cleanedText, "[\r\n]+", " ") ; Replace line breaks with spaces
        cleanedText := RegExReplace(cleanedText, "\s{2,}", " ") ; Replace multiple spaces with a single space
        cleanedText := Trim(cleanedText) ; Remove leading/trailing spaces
        
        ; For debugging - display the captured text and coordinates
        ToolTip "OCR area: x=" x ", y=" y ", w=" w ", h=" h "`nExtracted text: " cleanedText
        Sleep 1500
        ToolTip
        
        ; If we got text, return it
        if (StrLen(cleanedText) > 10) {
            return cleanedText
        }
        
        ; If the first attempt failed, try with increased height and shifted position
        x := 170       ; Keep x position
        y := 370       ; Start higher up
        h := 160       ; Larger height to capture more potential text
        
        ; Highlight the new capture area
        HighlightArea(x, y, w, h)
        
        ; Try OCR again with adjusted area
        ocrResult := OCR.FromRect(x, y, w, h, {scale: 3})
        fullText := ocrResult.Text
        
        ; Clean up the text
        cleanedText := RegExReplace(fullText, "[^\w\s]", "")
        cleanedText := RegExReplace(cleanedText, "[\r\n]+", " ")
        cleanedText := RegExReplace(cleanedText, "\s{2,}", " ")
        cleanedText := Trim(cleanedText)
        
        ; Debug info with coordinates
        ToolTip "Second OCR attempt: x=" x ", y=" y ", w=" w ", h=" h "`nExtracted text: " cleanedText
        Sleep 1500
        ToolTip
        
        if (StrLen(cleanedText) > 10) {
            return cleanedText
        }
        
        ; If all else fails, use the text from the screenshot
        MsgBox "OCR couldn't extract enough text. Using fallback text."
        return "last write back he can end without can seem only about most here over who real around not where never own only small plan take general long be hand any last public might begin other become first at long during go since part open how that real little in get such may not no on face and little after it hold some tell little seem much high of late never these"
    } catch Error as e {
        ; If OCR fails, return the hardcoded text from the screenshot
        MsgBox "OCR failed. Error: " e.Message
        return "last write back he can end without can seem only about most here over who real around not where never own only small plan take general long be hand any last public might begin other become first at long during go since part open how that real little in get such may not no on face and little after it hold some tell little seem much high of late never these"
    }
}

; Helper function to visually highlight the area being OCR'ed
HighlightArea(x, y, w, h) {
    ; Create a GUI that will serve as a highlight border
    highlight := Gui("+AlwaysOnTop -Caption +ToolWindow +E0x20")
    highlight.BackColor := "33FF33"  ; Light green, semi-transparent
    highlight.Show("x" x " y" y " w" w " h" h " NoActivate")
    Sleep 300  ; Show the highlight briefly
    highlight.Destroy()
}

; --- Anti-Cheat Evasion Settings ---
; These values are based on Monkeytype's detection thresholds
keySpacingVarianceMin := 0.32   ; Even higher variance to appear more human-like (was 0.28)
keyDurationVarianceMin := 0.30  ; Even higher variance in key durations (was 0.25)
wpmConsistencyNoise := 0.085    ; More noise to appear human (was 0.065)
minKeysBeforeEval := 18         ; More keys before evaluating (was 15)
maxKeySpacingOutlier := 650     ; Less extreme but still variable (was 450)
maxKeyDurationOutlier := 220    ; Less extreme but still variable (was 150)
keyOverlapTargetPct := 0.075    ; More realistic overlap (was 0.09)
intraWordDelayChance := 0.045   ; More delays within words for realism (was 0.03)
interWordFasterChance := 0.72   ; More variable between-word speed (was 0.82)

; --- Stats Analysis Functions (for Anti-Cheat Evasion) ---
CalculateMean(array) {
    sum := 0
    for value in array {
        sum += value
    }
    return sum / array.Length
}

CalculateStdDev(array) {
    mean := CalculateMean(array)
    
    varianceSum := 0
    for value in array {
        varianceSum += (value - mean) * (value - mean)
    }
    
    variance := varianceSum / array.Length
    stdDev := Sqrt(variance)
    
    return stdDev
}

CalculateCV(array) {
    if (array.Length < 2)
        return 1.0
    
    mean := CalculateMean(array)
    if (mean = 0)
        return 1.0
    
    stdDev := CalculateStdDev(array)
    return stdDev / mean
}

; --- Main Typing Loop ---
SimulateTyping() {
    global totalCharsTyped, startTime, lastCharTime, fingerFatigue
    global typistProfile, burstModeActive, currentBurstSpeed, consecutiveCorrect
    global testDuration, endSprintStart, endSprintFactor, targetWPM, patternStartTime
    global currentPatternIndex, lastErrorTime, lastHesitationTime, lastPauseWordIndex
    global wordCount, naturalTypingFlow, lastFiveKeyTimes, humanInconsistencyFactor
    global keyHeldLength, lastLetterTiming, keyPressStagger, lastMicroPause, handFatigueAmount
    global pauseAfterErrorChance, nonUniformVariability, realHumanFluctuation
    global keyDurationVariance, keyOverlapProbability, lastKeyShortenFactor
    
    ; Reset state and timing
    currentPatternIndex := 1
    patternStartTime := A_TickCount
    
    ; Initialize timing trackers
    Loop 5 {
        lastFiveKeyTimes[A_Index] := 0
    }
    
    ; Get text to type using OCR
    text := GetMonkeytypeText()
    ToolTip "OCR extracted text: " text
    Sleep 400  ; Reduced from 800 for faster startup
    ToolTip
    
    ; Initialize text length and position variables
    n := StrLen(text)        ; Get length of text to type
    i := 1                   ; Initialize position counter
    prevChar := ""          ; Initialize previous character tracker
    
    ; Reset all tracking variables
    Loop 5 {
        lastFiveKeyTimes[A_Index] := 0
    }
    totalCharsTyped := 0
    startTime := A_TickCount
    lastCharTime := startTime
    burstModeActive := false
    currentBurstSpeed := 1.0
    consecutiveCorrect := 0
    lastErrorTime := 0
    lastHesitationTime := 0
    lastPauseWordIndex := 0
    wordCount := 0
    naturalTypingFlow := true
    humanInconsistencyFactor := 1.0
    keyHeldLength := 0
    lastLetterTiming := 0
    keyPressStagger := false
    lastMicroPause := 0
    handFatigueAmount := 0
    nonUniformVariability := 0
    pauseAfterErrorChance := 0
    realHumanFluctuation := 0
    keyDurationVariance := 0
    keyOverlapProbability := 0
    lastKeyShortenFactor := 1.0
    
    ; Reset finger fatigue
    for key in fingerFatigue {
        fingerFatigue[key] := 0
    }
    
    ; ===== ANTI-CHEAT EVASION VARIABLES =====
    ; These track key metrics that anticheat systems analyze
    keySpacingValues := []    ; Store keystroke timing intervals
    keyDurationValues := []   ; Store key hold durations
    lastKeyTime := 0          ; When the last key was pressed
    typingPhase := 0          ; 0=starting, 1=regular, 2=ending
    burstPhase := 0           ; 0-5 scale of current typing burst phase
    lastBurstChange := 0      ; When we last changed burst phase
    keySpacingMean := 0       ; Average time between keystrokes
    keyDurationMean := 0      ; Average key hold time
    keyCount := 0             ; Number of keys typed
    
    ; Initialize with ultra-realistic human preparation
    initialDelay := Rand(250, 450)  ; More realistic initial delay (was 150-300)
    
    ; Simulate reading/scanning the text before starting (critical for realistic appearance)
    ToolTip "Reading text..."
    Sleep initialDelay
    
    ; Add a realistic finger positioning adjustment and mental preparation
    positioningDelay := Rand(80, 160)  ; More realistic positioning (was 50-100)
    Sleep positioningDelay
    
    ; Sometimes add a tiny last moment hesitation before starting (very human)
    if (Rand(1, 3) <= 2) {
        lastMomentDelay := Rand(40, 120)  ; More realistic hesitation (was 20-70)
        Sleep lastMomentDelay
    }
    
    ; Add variable personality-based starting speed (humans don't start at full speed)
    startingSpeedFactor := 1.0 + (Rand(3, 6) / 100)  ; More realistic slowdown (was 0-2%)
    
    ; First characters are always less consistent as typist settles in
    firstCharRandomness := 0.18  ; Higher initial randomness (was 0.12)
    
    ; Main typing loop with ultra-realistic human behavior simulation
    ToolTip "Typing started..."
    while (i <= n && A_TickCount - startTime < testDuration * 1000) {
        ; Track time for human-like pacing
        elapsedTime := A_TickCount - startTime
        elapsedSec := elapsedTime / 1000
        elapsedPct := elapsedSec / testDuration
        
        ; Current character 
        currentChar := SubStr(text, i, 1)
        if (currentChar = "")
            break
            
        ; Get current word context
        currentWord := GetCurrentWord(text, i)
        
        ; Determine typing phase (for realistic typing curve)
        if (elapsedPct < 0.15)
            typingPhase := 0  ; Starting phase - more variable, slower
        else if (elapsedPct > 0.85)
            typingPhase := 2  ; Ending phase - varies by person
        else
            typingPhase := 1  ; Regular typing phase
            
        ; Vary burst phase periodically (creates natural WPM chart pattern)
        ; This is CRITICAL for bypassing Monkeytype's WPM consistency check
        if ((A_TickCount - lastBurstChange > 1200 && Rand(1, 15) = 1) ||  ; More frequent changes (was 1500, 20)
            (burstPhase = 0 && elapsedPct > 0.15)) {  ; Earlier progression (was 0.2)
            
            prevBurst := burstPhase
            
            ; Realistic burst transitions - rarely make huge jumps
            if (Rand(1, 10) <= 7) {
                ; Small transition (common)
                if (burstPhase < 5 && Rand(1, 2) = 1)
                    burstPhase += 1
                else if (burstPhase > 0)
                    burstPhase -= 1
            } else {
                ; Larger transition (uncommon)
                burstPhase := Rand(0, 5)
            }
            
            ; Add transition effects that humans show
            if (prevBurst != burstPhase && Rand(1, 4) = 1) {  ; Less likely (was 1 in 3)
                ; Brief adjustment when changing typing speed
                Sleep Rand(10, 40)  ; Shorter pause (was 20-60)
            }
            
            lastBurstChange := A_TickCount
        }
        
        ; Apply phase-specific timing changes
        burstMultiplierValue := 1.0
        if (burstPhase = 0) {
            burstMultiplierValue := 0.98  ; More realistic slowdown (was 0.92)
        } else if (burstPhase = 1) {
            burstMultiplierValue := 0.94  ; More realistic (was 0.87)
        } else if (burstPhase = 2) {
            burstMultiplierValue := 0.90  ; More realistic (was 0.82)
        } else if (burstPhase = 3) {
            burstMultiplierValue := 0.85  ; More realistic (was 0.78)
        } else if (burstPhase = 4) {
            burstMultiplierValue := 0.80  ; More realistic (was 0.72)
        } else if (burstPhase = 5) {
            burstMultiplierValue := 0.75  ; More realistic (was 0.68)
        }
        
        ; Apply human-like starting speed curve
        phaseFactor := 1.0
        if (typingPhase = 0) {
            ; Starting phase adjustment
            startProgress := elapsedPct / 0.15
            phaseFactor := startingSpeedFactor - (startProgress * (startingSpeedFactor - 1.0))
            
            ; Add higher randomness at beginning
            if (i < 15) {
                randomnessFactor := (15 - i) / 15
                phaseFactor *= 1 + (Rand(-10, 10) / 100) * randomnessFactor
            }
        } else if (typingPhase = 2) {
            ; End phase adjustment varies by person
            endStyle := Rand(1, 10)
            if (endStyle <= 6) {
                ; Speed up (common)
                endProgress := (elapsedPct - 0.85) / 0.15
                phaseFactor := 1.0 - (endProgress * 0.12)
            } else {
                ; Maintain or slightly slow (less common)
                phaseFactor := 1.0 + (Rand(-3, 5) / 100)
            }
        }
        
        ; Handle space key differently - humans have distinctive space patterns
        if (currentChar = " ") {
            ; Humans press space differently than letter keys
            keyHeldLength := Rand(25, 50)  ; More realistic space bar timing (was 20-40)
            
            ; Apply timing multipliers
            spaceTimingAdjustment := phaseFactor * burstMultiplierValue * 0.9  ; 10% faster spaces (new)
            
            ; Ensure variable key hold times (critical for anticheat evasion)
            if (keyDurationValues.Length >= 5) {
                durationMean := 0
                for dur in keyDurationValues
                    durationMean += dur
                durationMean /= keyDurationValues.Length
                
                ; Ensure our durations aren't too consistent (would trigger anticheat)
                durationVariance := 0
                for dur in keyDurationValues
                    durationVariance += (dur - durationMean) * (dur - durationMean)
                durationVariance /= keyDurationValues.Length
                durationStdDev := Sqrt(durationVariance)
                
                ; Calculate coefficient of variation - botched typing has CV < 0.15
                durationCV := durationStdDev / durationMean
                
                ; Adjust if needed
                if (durationCV < 0.18) {
                    ; Too consistent - vary more
                    keyHeldLength *= 1 + (Rand(-20, 20) / 100)
                }
            }
            
            ; Tracking for anticheat-evading timing patterns
            preKeyTime := A_TickCount
            
            ; Actual key typing with variable hold time
            SendInput "{Space down}"
            Sleep keyHeldLength
            SendInput "{Space up}"
            
            ; Record key duration
            keyDurationValues.Push(keyHeldLength)
            if (keyDurationValues.Length > 20)
                keyDurationValues.RemoveAt(1)
                
            ; Track timing between keys (critical for anti-cheat evasion)
            postKeyTime := A_TickCount
            if (lastKeyTime > 0) {
                keySpacing := preKeyTime - lastKeyTime
                keySpacingValues.Push(keySpacing)
                if (keySpacingValues.Length > 20)
                    keySpacingValues.RemoveAt(1)
                    
                ; Update mean
                keySpacingMean := 0
                for spacing in keySpacingValues
                    keySpacingMean += spacing
                keySpacingMean /= keySpacingValues.Length
            }
            lastKeyTime := postKeyTime
            
            totalCharsTyped++
            keyCount++
            consecutiveCorrect++
            
            ; Space timing is different than regular keys
            spaceDelay := GetHumanDelay(text, i, currentChar) * spaceTimingAdjustment * 0.92  ; More realistic (was 0.85)
            
            ; Check if our timing pattern is "too clean" for a human
            if (keySpacingValues.Length >= 10) {
                ; Calculate variance to ensure we look human
                spacingVariance := 0
                for spacing in keySpacingValues
                    spacingVariance += (spacing - keySpacingMean) * (spacing - keySpacingMean)
                spacingVariance /= keySpacingValues.Length
                spacingStdDev := Sqrt(spacingVariance)
                
                ; Calculate coefficient of variation
                spacingCV := spacingStdDev / keySpacingMean
                
                ; If timing is too consistent, add variation
                if (spacingCV < 0.22) {
                    ; Too robot-like, add human variance
                    extraVariance := Rand(20, 60)
                    Sleep extraVariance
                }
            }
            
            Sleep spaceDelay
            } else {
            ; Process regular character with sophisticated human error model
            errorResult := GenerateHumanError(currentChar, text, i, elapsedTime)
            
            ; Humans recover from rhythm disruptions in unique ways
            if (!naturalTypingFlow && Rand(1, 10) <= 7) {
                ; Hesitation during recovery
                Sleep Rand(8, 20)
                
                ; Return to flow gradually
                if (consecutiveCorrect > 3) {
                    naturalTypingFlow := true
                }
            }
            
            ; Apply appropriate key press based on error model
            if (errorResult["char"] != currentChar && errorResult["char"] != "") {
                ; Made a typing error - human key press changes during errors
                keyHeldLength := Rand(10, 20)  ; Faster (was 35-55)
                
                ; Record timing before keypress
                preKeyTime := A_TickCount
                
                ; Type the incorrect key
                TypeKey(errorResult["char"])
                
                ; Record key duration and update statistics
                keyDurationValues.Push(keyHeldLength)
                if (keyDurationValues.Length > 20)
                    keyDurationValues.RemoveAt(1)
                
                ; Track timing between keys
                postKeyTime := A_TickCount
                if (lastKeyTime > 0) {
                    keySpacing := preKeyTime - lastKeyTime
                    keySpacingValues.Push(keySpacing)
                    if (keySpacingValues.Length > 20)
                        keySpacingValues.RemoveAt(1)
                }
                lastKeyTime := postKeyTime
                
                totalCharsTyped++
                keyCount++
                
                if (errorResult["correct"]) {
                    ; Error correction with sophisticated human pattern
                    addedChars := SimulateRealisticErrorCorrection(errorResult, currentChar)
                    totalCharsTyped += addedChars
                    keyCount += addedChars
                    consecutiveCorrect := 2
                    
                    ; After correction, timing is often disrupted
                    lastKeyTime := A_TickCount
                    
                    ; After error correction, realistic flow disruption
                    if (Rand(1, 10) <= 5) {  ; More likely (was 3 in 10)
                        naturalTypingFlow := false
                        
                        ; Humans often pause briefly after correcting errors
                        pauseAfterErrorChance := Rand(1, 100)
                        if (pauseAfterErrorChance <= 30) {  ; More likely (was 20)
                            Sleep Rand(40, 100)  ; Longer pause (was 30-80)
                        } else if (pauseAfterErrorChance <= 60) {  ; More likely (was 40)
                            Sleep Rand(20, 60)  ; Longer pause (was 10-40)
                        }
            }
        } else {
                    ; Uncorrected error
                    consecutiveCorrect := 0
                    
                    ; Humans sometimes slow down after uncorrected errors
                    if (Rand(1, 2) = 1) {
                        humanInconsistencyFactor *= 1.04 + (Rand(0, 5) / 100)
                    }
                }
            } 
            else if (errorResult["char"] = "") {
                ; Omission error - skipped character
                if (errorResult["correct"]) {
                    ; But we caught it and will correct with human pattern
                    addedChars := SimulateRealisticErrorCorrection(errorResult, currentChar)
                    totalCharsTyped += addedChars
                    keyCount += addedChars
                    consecutiveCorrect := 2
                    
                    ; Update timing
                    lastKeyTime := A_TickCount
                    
                    ; Humans often have trouble getting back in rhythm after omission errors
                    if (Rand(1, 3) <= 2) {
                        naturalTypingFlow := false
                        Sleep Rand(30, 70)
                    }
                } else {
                    ; Skip character entirely (uncorrected omission)
                    consecutiveCorrect := 0
                }
            }
            else {
                ; Correct typing with sophisticated human key timing
                ; Calculate hold time based on key and position
                if (StrLen(currentWord) <= 3 && Rand(1, 10) <= 7) {
                    ; Short words are typed with more consistent timing
                    keyHeldLength := Rand(18, 33)
                } else if (IsLeadingPosition(text, i)) {
                    ; First letter of word
                    keyHeldLength := Rand(22, 45)
                } else if (IsTrailingPosition(text, i)) {
                    ; Last letter of word
                    keyHeldLength := Rand(15, 35)
                } else {
                    ; Middle of word
                    keyHeldLength := Rand(18, 38)
                }
                
                ; Apply adjustments for key type
                if (InStr("asdfjkl;", currentChar))
                    keyHeldLength *= 0.9 + (Rand(-5, 5) / 100)  ; Home row
                else if (InStr("zxqpywbm", currentChar))
                    keyHeldLength *= 1.1 + (Rand(-5, 8) / 100)  ; Uncommon keys
                
                ; Adjust for burst phase
                keyHeldLength *= 0.95 + (0.05 * (5 - burstPhase))
                
                ; Record timing before keypress
                preKeyTime := A_TickCount
                
                ; Type the key
            TypeKey(currentChar)
                
                ; Record key duration and update statistics
                keyDurationValues.Push(keyHeldLength)
                if (keyDurationValues.Length > 20)
                    keyDurationValues.RemoveAt(1)
                
                ; Track timing between keys
                postKeyTime := A_TickCount
                if (lastKeyTime > 0) {
                    keySpacing := preKeyTime - lastKeyTime
                    keySpacingValues.Push(keySpacing)
                    if (keySpacingValues.Length > 20)
                        keySpacingValues.RemoveAt(1)
                }
                lastKeyTime := postKeyTime
                
            totalCharsTyped++
                keyCount++
                consecutiveCorrect++
                
                ; Humans have micro-stutters in timing
                if (Rand(1, 320) <= 1 && A_TickCount - lastErrorTime > 1500) {
                    microStutter := Rand(5, 25)
                    Sleep microStutter
                }
            }
        }
        
        ; Calculate human-like delay between keys
        delay := GetHumanDelay(text, i, currentChar)
        
        ; Apply phase adjustments
        delay *= burstMultiplierValue * phaseFactor
        
        ; Natural flow variance with realistic human patterns
        if (naturalTypingFlow) {
            ; Maintain flow with subtle individual variation
            if (Rand(1, 20) <= 2) {
                randomMod := Rand(-8, 8) / 100
                delay *= (1 + randomMod)
            }
        } else {
            ; Recovering from disruption with realistic pattern
            randomMod := Rand(-5, 15) / 100
            delay *= (1 + randomMod)
            
            ; Sometimes add a micro-hesitation during recovery
            if (Rand(1, 8) <= 2) {
                Sleep Rand(5, 15)
            }
        }
        
        ; Apply delay between keys
        if (elapsedPct > 0.3) {  ; Only apply faster typing after the first 30% of test
            delay *= 0.85  ; Global speedup after warmup phase (new)
        }
        Sleep delay
        
        ; Add natural rhythm variance at word boundaries
        if (IsLeadingPosition(text, i)) {
            ; Word starts have distinctive timing patterns
            if (Rand(1, 4) <= 2) {
                wordStartPause := Rand(3, 18)
                Sleep wordStartPause
            }
        }
        
        ; Anti-cheat evading technique - occasionally check timing pattern coherence
        if (i > 0 && (Mod(i, 40) = 0) && keySpacingValues.Length >= 10) {  ; Less frequent checks (was mod 30)
            ; Check if our timing patterns are too uniform (would trigger bot detection)
            
            ; Calculate mean and standard deviation for key spacing
            meanSpacing := 0
            for spacing in keySpacingValues
                meanSpacing += spacing
            meanSpacing /= keySpacingValues.Length
            
            ; Calculate variance
            varianceSpacing := 0
            for spacing in keySpacingValues
                varianceSpacing += (spacing - meanSpacing) * (spacing - meanSpacing)
            varianceSpacing /= keySpacingValues.Length
            stdDevSpacing := Sqrt(varianceSpacing)
            
            ; Calculate coefficient of variation (CV) - critical for avoiding detection
            ; Humans typically have CV between 0.2 and 0.4
            cvSpacing := stdDevSpacing / meanSpacing
            
            ; If our typing is too consistent, add some variation
            if (cvSpacing < 0.22) {
                ; Add natural inconsistency to avoid detection
                Sleep Rand(15, 80)
            }
            
            ; Also check key duration pattern
            if (keyDurationValues.Length >= 10) {
                meanDuration := 0
                for duration in keyDurationValues
                    meanDuration += duration
                meanDuration /= keyDurationValues.Length
                
                varianceDuration := 0
                for duration in keyDurationValues
                    varianceDuration += (duration - meanDuration) * (duration - meanDuration)
                varianceDuration /= keyDurationValues.Length
                stdDevDuration := Sqrt(varianceDuration)
                
                cvDuration := stdDevDuration / meanDuration
                
                ; Ensure key duration pattern looks human
                if (cvDuration < 0.18) {
                    ; Adjust next key duration to be more variable
                    keyHeldLength := Rand(15, 50)
                }
            }
        }
        
        ; Move to next character
        i++
    }
    
    ToolTip
    
    ; Humans have distinctive end-of-test behavior (crucial for realism)
    if (i > n && elapsedSec < testDuration) {
        ; Finished before time was up - add natural completion gestures
        Sleep Rand(200, 400)
        
        ; Human completion behaviors have patterns
        completionStyle := Rand(1, 10)
        
        if (completionStyle <= 4) {
            ; Style 1: Make a small adjustment after finishing (very human)
            SendInput "{Backspace}"
            Sleep Rand(40, 80)
            ; Type the last character again
            TypeKey(SubStr(text, n, 1))
            Sleep Rand(30, 70)
        } 
        else if (completionStyle <= 7) {
            ; Style 2: Just pause naturally
            Sleep Rand(150, 350)
        }
        else if (completionStyle <= 9) {
            ; Style 3: Double-check with arrow key (very human behavior)
            SendInput "{Left}"
            Sleep Rand(80, 150)
            SendInput "{Right}"
            Sleep Rand(60, 120)
        }
        else {
            ; Style 4: Mild fidgeting
            SendInput "{Left}"
            Sleep Rand(50, 100)
            SendInput "{Left}"
            Sleep Rand(60, 120)
            SendInput "{Right}"
            Sleep Rand(30, 80)
            SendInput "{Right}"
            Sleep Rand(40, 90)
        }
    } else {
        ; Time ran out - natural end-test behavior
        endTestBehavior := Rand(1, 5)
        
        if (endTestBehavior <= 3) {
            ; Most common: brief pause
            Sleep Rand(150, 350)
        } else if (endTestBehavior = 4) {
            ; Sometimes: quick check of last character
            SendInput "{Left}"
            Sleep Rand(60, 130)
            SendInput "{Right}"
            Sleep Rand(40, 100)
        } else {
            ; Rarely: mild relief gesture (stop typing abruptly)
            Sleep Rand(200, 400)
        }
    }
    
    ; Calculate final WPM
    elapsedTime := (A_TickCount - startTime) / 1000
    netChars := totalCharsTyped
    actualWPM := (netChars / 5) / (elapsedTime / 60) * 1.0  ; No artificial boost (was 1.08)
    
    return Round(actualWPM)
}

; --- Status Indicator ---
ShowStatus(message) {
    ToolTip message
    Sleep 1000
    ToolTip
}

; --- Focus Helper ---
ActivateMonkeytype() {
    ; Try to find and activate the Monkeytype window
    ; For Chrome
    if WinExist("ahk_exe chrome.exe") {
        WinActivate
        Sleep 500
        return true
    }
    ; For Firefox
    if WinExist("ahk_exe firefox.exe") {
        WinActivate
        Sleep 500
        return true
    }
    ; For Edge
    if WinExist("ahk_exe msedge.exe") {
        WinActivate
        Sleep 500
        return true
    }
    ; For other browsers
    if WinExist("monkeytype") {
        WinActivate
        Sleep 500
        return true
    }
    return false
}

; --- Hotkey Handler ---
Hotkey toggleKey, ToggleTyping

ToggleTyping(*) {
    global isTyping, totalCharsTyped, startTime
    
if (!isTyping) {
    isTyping := true
    
    ; Indicate we're starting
    ShowStatus("Starting in 3 seconds - Switch to Monkeytype NOW!")
        Sleep 3000
    
    ; Try to ensure Monkeytype is focused
    ActivateMonkeytype()
    
    ; Show we're ready to type
    ShowStatus("Starting typing NOW...")
        Sleep 500
    
    ; Run the typing simulation
    actualWPM := SimulateTyping()
    
    ; Show final WPM
        ToolTip "Completed! " actualWPM " WPM"
        Sleep 2000
    ToolTip
    
    isTyping := false
} else {
    ShowStatus("Already typing...")
}
}

; --- Adjacent Key Map Function ---
GetAdjacentKeys() {
    return Map(
        "q", ["w", "a", "1", "2"],
        "w", ["q", "e", "s", "a", "2", "3"],
        "e", ["w", "r", "d", "s", "3", "4"],
        "r", ["e", "t", "f", "d", "4", "5"],
        "t", ["r", "y", "g", "f", "5", "6"],
        "y", ["t", "u", "h", "g", "6", "7"],
        "u", ["y", "i", "j", "h", "7", "8"],
        "i", ["u", "o", "k", "j", "8", "9"],
        "o", ["i", "p", "l", "k", "9", "0"],
        "p", ["o", "[", ";", "l", "0", "-"],
        "a", ["q", "w", "s", "z"],
        "s", ["w", "e", "d", "x", "z", "a"],
        "d", ["e", "r", "f", "c", "x", "s"],
        "f", ["r", "t", "g", "v", "c", "d"],
        "g", ["t", "y", "h", "b", "v", "f"],
        "h", ["y", "u", "j", "n", "b", "g"],
        "j", ["u", "i", "k", "m", "n", "h"],
        "k", ["i", "o", "l", ",", "m", "j"],
        "l", ["o", "p", ";", ".", ",", "k"],
        ";", ["p", "[", "'", "/", ".", "l"],
        "z", ["a", "s", "x"],
        "x", ["s", "d", "c", "z"],
        "c", ["d", "f", "v", "x"],
        "v", ["f", "g", "b", "c"],
        "b", ["g", "h", "n", "v"],
        "n", ["h", "j", "m", "b"],
        "m", ["j", "k", ",", "n"],
        ",", ["k", "l", ".", "m"],
        ".", ["l", ";", "/", ","],
        "/", [";", "'", "."],
        "'", [";", "[", "/"],
        "[", ["p", "]", "'"],
        "]", ["[", "\", "="],
        "\", ["]", "="],
        "1", ["q", "2"],
        "2", ["1", "q", "w", "3"],
        "3", ["2", "w", "e", "4"],
        "4", ["3", "e", "r", "5"],
        "5", ["4", "r", "t", "6"],
        "6", ["5", "t", "y", "7"],
        "7", ["6", "y", "u", "8"],
        "8", ["7", "u", "i", "9"],
        "9", ["8", "i", "o", "0"],
        "0", ["9", "o", "p", "-"],
        "-", ["0", "p", "[", "="],
        "=", ["-", "[", "]"]
    )
}