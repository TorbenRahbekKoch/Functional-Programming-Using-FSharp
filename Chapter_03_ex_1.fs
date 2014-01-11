module Chapter_03_ex_1

// 3.1 time test

// Define an enum and a record type to hold a time.
type Meridiem = AM | PM

type Time = {
    AmPm : Meridiem
    Hour : int;
    Minute : int;
}

// Due to the built-in value comparison of records with comparable members
// the comparison of Time records doesn't need any specific implementation, since
// we have ordered the members deliberately to reflect that AmPm is more important than Hour, etc.
// This would also have worked if we used a string ("AM"/"PM") for the Meridiem.
// If the record had been defined as below, it would not work:

type Time2 = {
    Minute2 : int
    Hour2 : int
    AmPm2 : Meridiem
}

// Try running this in Interactive (together with Time2 and Meridiem definition above) - it results
// in the value true, which is obviously wrong:
let result2 = { AmPm2 = AM; Hour2=11; Minute2=10 } < { AmPm2 = AM; Hour2=10; Minute2=11 }

// The triple definition, on the other hand, does need a specific function to compare, when it is defined
// as in the book with (hour, minute, meridiem) - that we are using a string for meridiem does not matter
// since "AM" does sort before "PM".
let lessThan (time1: int*int*string) (time2: int*int*string) =
    let (hour1, minute1, amPm1) = time1
    let (hour2, minute2, amPm2) = time2
    if (amPm1 < amPm2) then
        true
    elif (amPm1 = amPm2 && hour1 < hour2) then
        true
    elif (amPm1 = amPm2 && hour1 = hour2 && minute1 < minute2) then
        true
    else
        false

let (&<) time1 time2 = lessThan time1 time2

// If we were using (meridiem, hour, minute) instead, it would work out of the box:
let result3a = ("AM", 11, 10) < ("PM", 11, 10) // true
let result3b = ("AM", 11, 10) < ("AM", 11, 11) // true
let result3c = ("AM", 11, 10) < ("AM", 11, 9)  // false
    
// We have now learned that F# uses the definition order of the record/tuple-members to compare by.
// Very nice, tidy and logical! :)

