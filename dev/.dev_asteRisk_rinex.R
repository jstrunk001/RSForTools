rnxHdr = function (lines) {
  
    line1 <- lines[1]
    line2 <- lines[2]
    rinexVersion <- trimws(substr(line1, 1, 9))
    rinexFileType <- trimws(substr(line1, 21, 60))
    generatorProgram <- trimws(substr(line2, 1, 20))
    generatorEntity <- trimws(substr(line2, 21, 40))
    fileCreationDateString <- trimws(substr(line2, 41, 60))
    commentLinesIndexes <- grep("COMMENT", lines)
    if (length(commentLinesIndexes) > 0) {
        comments <- trimws(substr(lines[commentLinesIndexes], 
            1, 60))
    }
    else {
        comments <- NULL
    }
    ionAlphaLineIndex <- grep("ION ALPHA", lines)
    if (length(ionAlphaLineIndex) > 0) {
        ionAlphaLine <- lines[ionAlphaLineIndex]
        ionAlphaFields <- strsplit(trimws(substr(ionAlphaLine, 
            1, 60)), split = "\\s+")[[1]]
        ionAlphaA0 <- as.numeric(gsub("D", "E", ionAlphaFields[1]))
        ionAlphaA1 <- as.numeric(gsub("D", "E", ionAlphaFields[2]))
        ionAlphaA2 <- as.numeric(gsub("D", "E", ionAlphaFields[3]))
        ionAlphaA3 <- as.numeric(gsub("D", "E", ionAlphaFields[4]))
    }
    else {
        ionAlphaA0 <- ionAlphaA1 <- ionAlphaA2 <- ionAlphaA3 <- NULL
    }
    ionBetaLineIndex <- grep("ION BETA", lines)
    if (length(ionBetaLineIndex) > 0) {
        ionBetaLine <- lines[ionBetaLineIndex]
        ionBetaFields <- strsplit(trimws(substr(ionBetaLine, 
            1, 60)), split = "\\s+")[[1]]
        ionBetaB0 <- as.numeric(gsub("D", "E", ionBetaFields[1]))
        ionBetaB1 <- as.numeric(gsub("D", "E", ionBetaFields[2]))
        ionBetaB2 <- as.numeric(gsub("D", "E", ionBetaFields[3]))
        ionBetaB3 <- as.numeric(gsub("D", "E", ionBetaFields[4]))
    }
    else {
        ionBetaB0 <- ionBetaB1 <- ionBetaB2 <- ionBetaB3 <- NULL
    }
    deltaUTCLineIndex <- grep("DELTA.UTC", lines)
    if (length(deltaUTCLineIndex) > 0) {
        deltaUTCLine <- lines[deltaUTCLineIndex]
        deltaUTCA0 <- as.numeric(gsub("D", "E", substr(deltaUTCLine, 
            1, 22)))
        deltaUTCA1 <- as.numeric(gsub("D", "E", substr(deltaUTCLine, 
            23, 42)))
        referenceTimeUTC <- as.numeric(substr(deltaUTCLine, 43, 
            51))
        referenceWeekUTC <- as.numeric(substr(deltaUTCLine, 52, 
            60))
    }
    else {
        deltaUTCLine <- deltaUTCA0 <- deltaUTCA1 <- referenceTimeUTC <- referenceWeekUTC <- NULL
    }
    leapSecondsLineIndex <- grep("LEAP SECONDS", lines)
    if (length(leapSecondsLineIndex) > 0) {
        leapSeconds <- as.numeric(substr(lines[leapSecondsLineIndex], 
            1, 6))
    }
    else {
        leapSeconds <- NA
    }
    return(list(rinexVersion = rinexVersion, rinexFileType = rinexFileType, 
        generatorProgram = generatorProgram, generatorEntity = generatorEntity, 
        fileCreationDateString = fileCreationDateString, ionAlphaA0 = ionAlphaA0, 
        ionAlphaA1 = ionAlphaA1, ionAlphaA2 = ionAlphaA2, ionAlphaA3 = ionAlphaA3, 
        ionBetaB0 = ionBetaB0, ionBetaB1 = ionBetaB1, ionBetaB2 = ionBetaB2, 
        ionBetaB3 = ionBetaB3, deltaUTCA0 = deltaUTCA0, deltaUTCA1 = deltaUTCA1, 
        referenceTimeUTC = referenceTimeUTC, referenceWeekUTC = referenceWeekUTC, 
        leapSeconds = leapSeconds, comments = comments))
    
}

parse_nav = function (lines, leapSeconds = 0, deltaUTCA0 = 0, deltaUTCA1 = 0, 
    referenceTimeUTC, referenceWeekUTC) {
    if (length(lines) != 8) {
        stop("Invalid GPS navigation RINEX file")
    }
    if (is.na(leapSeconds)) 
        leapSeconds <- 0
    line1 <- gsub("D", "E", lines[1], ignore.case = TRUE)
    line2 <- gsub("D", "E", lines[2], ignore.case = TRUE)
    line3 <- gsub("D", "E", lines[3], ignore.case = TRUE)
    line4 <- gsub("D", "E", lines[4], ignore.case = TRUE)
    line5 <- gsub("D", "E", lines[5], ignore.case = TRUE)
    line6 <- gsub("D", "E", lines[6], ignore.case = TRUE)
    line7 <- gsub("D", "E", lines[7], ignore.case = TRUE)
    line8 <- gsub("D", "E", lines[8], ignore.case = TRUE)
    satellitePRNCode <- as.numeric(substr(line1, 1, 2))
    tocYearShort <- as.numeric(substr(line1, 4, 5))
    tocMonth <- as.numeric(substr(line1, 7, 8))
    tocDay <- as.numeric(substr(line1, 10, 11))
    tocHour <- as.numeric(substr(line1, 13, 14))
    tocMinute <- as.numeric(substr(line1, 16, 17))
    tocSecond <- as.numeric(substr(line1, 18, 22))
    clockBias <- as.numeric(substr(line1, 23, 41))
    clockDrift <- as.numeric(substr(line1, 42, 60))
    clockDriftRate <- as.numeric(substr(line1, 61, 79))
    IODE <- as.numeric(substr(line2, 4, 22))
    radiusCorrectionSine <- as.numeric(substr(line2, 23, 41))
    deltaN <- as.numeric(substr(line2, 42, 60))
    meanAnomaly <- as.numeric(substr(line2, 61, 79))
    latitudeCorrectionCosine <- as.numeric(substr(line3, 4, 22))
    eccentricity <- as.numeric(substr(line3, 23, 41))
    latitudeCorrectionSine <- as.numeric(substr(line3, 42, 60))
    semiMajorAxis <- as.numeric(substr(line3, 61, 79))^2
    calculatedMeanMotion <- asteRisk:::semiMajorAxisToMeanMotion(semiMajorAxis, 
        outputRevsPerDay = FALSE)
    correctedMeanMotion <- calculatedMeanMotion + deltaN
    toeSecondsOfGPSWeek <- as.numeric(substr(line4, 4, 22))
    inclinationCorrectionCosine <- as.numeric(substr(line4, 23, 
        41))
    ascension <- as.numeric(substr(line4, 42, 60))
    inclinationCorrectionSine <- as.numeric(substr(line4, 61, 
        79))
    inclination <- as.numeric(substr(line5, 4, 22))
    radiusCorrectionCosine <- as.numeric(substr(line5, 23, 41))
    perigeeArgument <- as.numeric(substr(line5, 42, 60))
    OMEGADot <- as.numeric(substr(line5, 61, 79))
    inclinationRate <- as.numeric(substr(line6, 4, 22))
    codesL2Channel <- as.numeric(substr(line6, 23, 41))
    toeGPSWeek <- as.numeric(substr(line6, 42, 60))
    dataFlagL2P <- as.numeric(substr(line6, 61, 79))
    satelliteAccuracy <- as.numeric(substr(line7, 4, 22))
    satelliteHealthCode <- as.numeric(substr(line7, 23, 41))
    totalGroupDelay <- as.numeric(substr(line7, 42, 60))
    IODC <- as.numeric(substr(line7, 61, 79))
    transmissionTime <- as.numeric(substr(line8, 4, 22))
    fitInterval <- as.numeric(substr(line8, 23, 41))
    browser()
    if (is.numeric(tocYearShort)) 
        if (tocYearShort >= 80) {
            tocYear <- 1900 + tocYearShort
        }
        else {
            tocYear <- 2000 + tocYearShort
        }
    tocDateTimeString <- paste(tocYear, "-", tocMonth, 
        "-", tocDay, " ", tocHour, ":", tocMinute, 
        ":", tocSecond, sep = "")
    tocGPSTseconds <- as.double(difftime(tocDateTimeString, "1980-01-06 00:00:00", 
        tz = "UTC", units = "secs"))
    tocGPSWeek <- tocGPSTseconds%/%604800
    tocSecondsOfGPSWeek <- tocGPSTseconds%%604800
    differenceToeToc <- toeSecondsOfGPSWeek - tocSecondsOfGPSWeek
    if (differenceToeToc > 302400) 
        differenceToeToc <- differenceToeToc - 604800
    if (differenceToeToc < -302400) 
        differenceToeToc <- differenceToeToc + 604800
    F_constant <- -2 * sqrt(GM_Earth_TDB)/c_light^2
    kepler_sol_1 <- meanAnomaly
    kepler_sol_current <- kepler_sol_1
    convergence <- FALSE
    iterations <- 0
    keplerAccuracy = 0.0000001
    maxKeplerIterations = 100
    while (!convergence) {
        iterations <- iterations + 1
        delta_kepler_sol <- ((meanAnomaly - kepler_sol_current + 
            eccentricity * sin(kepler_sol_current))/(1 - eccentricity * 
            cos(kepler_sol_current)))
        kepler_sol_current <- kepler_sol_current + delta_kepler_sol
        if (iterations > maxKeplerIterations | delta_kepler_sol < 
            keplerAccuracy) {
            convergence <- TRUE
        }
    }
    eccentricAnomaly <- kepler_sol_current
    relativityDeltaSVtimeToGPST <- F_constant * eccentricity * 
        (sqrt(semiMajorAxis)) * sin(eccentricAnomaly)
    deltaSVtimeToGPST <- clockBias + clockDrift * differenceToeToc + 
        clockDriftRate * differenceToeToc^2 + relativityDeltaSVtimeToGPST
    toeSecondsOfGPSWeekGPST <- toeSecondsOfGPSWeek - deltaSVtimeToGPST
    differenceToeGPSTTot <- toeSecondsOfGPSWeekGPST - referenceTimeUTC
    deltaGPSTToUTC <- leapSeconds + deltaUTCA0 + deltaUTCA1 * 
        (differenceToeGPSTTot + 604800 * (tocGPSWeek - referenceWeekUTC))
    ephemerisUTCDateTime <- as.POSIXct(toeGPSWeek * 604800 + 
        toeSecondsOfGPSWeek, origin = "1980-01-06 00:00:00", 
        tz = "UTC")
    ephemerisUTCDateTime <- as.nanotime(ephemerisUTCDateTime) - 
        deltaSVtimeToGPST * 1000000000 - deltaGPSTToUTC * 1000000000
    trueAnomaly <- atan2(sqrt(1 - eccentricity^2) * sin(eccentricAnomaly), 
        cos(eccentricAnomaly) - eccentricity)
    latitudeArgument <- trueAnomaly + perigeeArgument
    deltauk <- latitudeCorrectionSine * sin(2 * latitudeArgument) + 
        latitudeCorrectionCosine * cos(2 * latitudeArgument)
    deltark <- radiusCorrectionSine * sin(2 * latitudeArgument) + 
        radiusCorrectionCosine * cos(2 * latitudeArgument)
    deltaik <- inclinationCorrectionSine * sin(2 * latitudeArgument) + 
        inclinationCorrectionCosine * cos(2 * latitudeArgument)
    correctedLatitudeArgument <- latitudeArgument + deltauk
    correctedRadius <- semiMajorAxis * (1 - eccentricity * cos(eccentricAnomaly)) + 
        deltark
    correctedInclination <- inclination + deltaik
    xkprime <- correctedRadius * cos(correctedLatitudeArgument)
    ykprime <- correctedRadius * sin(correctedLatitudeArgument)
    correctedAscension <- ascension - omegaEarth * toeSecondsOfGPSWeek
    position_ECEF <- c(xkprime * cos(correctedAscension) - ykprime * 
        cos(correctedInclination) * sin(correctedAscension), 
        xkprime * sin(correctedAscension) + ykprime * cos(correctedInclination) * 
            cos(correctedAscension), ykprime * sin(correctedInclination))
    eccentricAnomalyDot <- correctedMeanMotion/(1 - eccentricity * 
        cos(eccentricAnomaly))
    trueAnomalyDot <- eccentricAnomalyDot * sqrt(1 - eccentricity^2)/(1 - 
        eccentricity * cos(eccentricAnomaly))
    correctedInclinationDot <- inclinationRate + 2 * trueAnomalyDot * 
        (inclinationCorrectionSine * cos(2 * latitudeArgument) - 
            inclinationCorrectionCosine * sin(2 * latitudeArgument))
    correctedLatitudeArgumentDot <- trueAnomalyDot + 2 * trueAnomalyDot * 
        (latitudeCorrectionSine * cos(2 * latitudeArgument) - 
            latitudeCorrectionCosine * sin(2 * latitudeArgument))
    correctedRadiusDot <- eccentricity * semiMajorAxis * eccentricAnomalyDot * 
        sin(eccentricAnomaly) + 2 * trueAnomalyDot * (radiusCorrectionSine * 
        cos(2 * latitudeArgument) - radiusCorrectionCosine * 
        sin(2 * latitudeArgument))
    ascensionDot <- OMEGADot - omegaEarth
    xkprimedot <- correctedRadiusDot * cos(correctedLatitudeArgument) - 
        correctedRadius * correctedLatitudeArgumentDot * sin(correctedLatitudeArgument)
    ykprimedot <- correctedRadiusDot * sin(correctedLatitudeArgument) + 
        correctedRadius * correctedLatitudeArgumentDot * cos(correctedLatitudeArgument)
    velocity_ECEF <- c(-xkprime * ascensionDot * sin(correctedAscension) + 
        xkprimedot * cos(correctedAscension) - ykprimedot * sin(correctedAscension) * 
        cos(correctedInclination) - ykprime * (ascensionDot * 
        cos(correctedAscension) * cos(correctedInclination) - 
        correctedInclinationDot * sin(correctedAscension) * sin(correctedInclination)), 
        xkprime * ascensionDot * cos(correctedAscension) + xkprimedot * 
            sin(correctedAscension) + ykprimedot * cos(correctedAscension) * 
            cos(correctedInclination) - ykprime * (ascensionDot * 
            sin(correctedAscension) * cos(correctedInclination) + 
            correctedInclinationDot * sin(correctedAscension) * 
                sin(correctedInclination)), ykprimedot * sin(correctedInclination) + 
            ykprime * correctedInclinationDot * cos(correctedInclination))
    oblateEarthAccelerationFactor <- -1.5 * J2_WGS84 * (GM_Earth_TDB/correctedRadius^2) * 
        (earthRadius_WGS84/correctedRadius)^2
    acceleration_ECEF <- c(-GM_Earth_TDB * (position_ECEF[1]/correctedRadius^3) + 
        oblateEarthAccelerationFactor * ((1 - 5 * (position_ECEF[3]/correctedRadius)^2) * 
            (position_ECEF[1]/correctedRadius)) + 2 * velocity_ECEF[2] * 
        omegaEarth + position_ECEF[1] * omegaEarth^2, -GM_Earth_TDB * 
        (position_ECEF[2]/correctedRadius^3) + oblateEarthAccelerationFactor * 
        ((1 - 5 * (position_ECEF[3]/correctedRadius)^2) * (position_ECEF[1]/correctedRadius)) - 
        2 * velocity_ECEF[1] * omegaEarth + position_ECEF[2] * 
        omegaEarth^2, -GM_Earth_TDB * (position_ECEF[3]/correctedRadius^3) + 
        oblateEarthAccelerationFactor * ((3 - 5 * (position_ECEF[3]/correctedRadius)^2) * 
            (position_ECEF[3]/correctedRadius)))
    return(list(satellitePRNCode = satellitePRNCode, tocYearShort = tocYearShort, 
        tocMonth = tocMonth, tocDay = tocDay, tocHour = tocHour, 
        tocMinute = tocMinute, tocSecond = tocSecond, clockBias = clockBias, 
        clockDrift = clockDrift, clockDriftRate = clockDriftRate, 
        IODE = IODE, radiusCorrectionSine = radiusCorrectionSine, 
        deltaN = deltaN, correctedMeanMotion = correctedMeanMotion, 
        meanAnomaly = meanAnomaly, latitudeCorrectionCosine = latitudeCorrectionCosine, 
        eccentricity = eccentricity, latitudeCorrectionSine = latitudeCorrectionSine, 
        semiMajorAxis = semiMajorAxis, toeSecondsOfGPSWeek = toeSecondsOfGPSWeek, 
        inclinationCorrectionCosine = inclinationCorrectionCosine, 
        ascension = ascension, inclinationCorrectionSine = inclinationCorrectionSine, 
        inclination = inclination, radiusCorrectionCosine = radiusCorrectionCosine, 
        perigeeArgument = perigeeArgument, OMEGADot = OMEGADot, 
        inclinationRate = inclinationRate, codesL2Channel = codesL2Channel, 
        toeGPSWeek = toeGPSWeek, dataFlagL2P = dataFlagL2P, satelliteAccuracy = satelliteAccuracy, 
        satelliteHealthCode = satelliteHealthCode, totalGroupDelay = totalGroupDelay, 
        IODC = IODC, transmissionTime = transmissionTime, fitInterval = fitInterval, 
        ephemerisUTCTime = ephemerisUTCDateTime, position_ITRF = position_ECEF, 
        velocity_ITRF = velocity_ECEF, acceleration_ITRF = acceleration_ECEF))
}

test_rnx =   function (filename) {
  
    lines <- readLines(filename)
    lines <- lines[lines != ""]
    endOfHeader <- grep("END OF HEADER", lines)
    headerLines <- lines[1:endOfHeader]
    bodyLines <- lines[(endOfHeader + 1):length(lines)]
    headerFields <- rnxHdr(headerLines)
    browser()
    messageNumberLines <- 8
    numberMessages <- length(bodyLines)/messageNumberLines
    parsedMessages <- vector(mode = "list", length = numberMessages)
    startingLines <- seq(1, by = messageNumberLines, length.out = numberMessages)
    for (i in 1:length(startingLines)) {
        singleMessageLines <- bodyLines[startingLines[i]:(startingLines[i] + 
            messageNumberLines - 1)]
        parsedMessages[[i]] <- parse_nav(singleMessageLines, 
            headerFields$leapSeconds, headerFields$deltaUTCA0, 
            headerFields$deltaUTCA1, headerFields$referenceTimeUTC, 
            headerFields$referenceWeekUTC)
    }
    
    return(list(header = headerFields, messages = parsedMessages))
    
  }
