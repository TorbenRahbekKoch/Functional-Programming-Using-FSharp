module Chapter_03_ex_2

// 3.2 with triple
// I'm note sure what is meant by "... use patterns to obtain readable declarations."

let addOldBritishMoney money1 money2 =
    let (pound1, shilling1, pence1) = money1
    let (pound2, shilling2, pence2) = money2
    let (pence, carry) = 
        let sum = pence1 + pence2
        (sum % 12, sum / 12)
    let (shilling, carry) =
        let sum = shilling1 + shilling2 + carry
        (sum % 20, sum / 20)
    let pound = pound1 + pound2 + carry
    (pound, shilling, pence)

let subtractOldBritishMoney money1 money2 =
    let (pound1, shilling1, pence1) = money1
    let (pound2, shilling2, pence2) = money2

    // Not sure it's called "carry" when subtracting...
    let (pence, carry) = 
        let difference = pence1 - pence2
        let willBorrow = difference < 0
        if willBorrow then (difference + 12, 1) else (difference, 0)

    let (shilling, carry) = 
        let difference = shilling1 - shilling2 - carry
        let willBorrow = difference < 0
        if willBorrow then (difference + 20, 1) else (difference, 0)

    let (pound, carry) =
        let difference = pound1 - pound2 - carry
        let willBorrow = difference < 0
        if willBorrow then (difference + 10, 1) else (difference, 0)

    if (carry > 0) then
        failwith "You're out of money!"
    (pound, shilling, pence)


let (&+) money1 money2 = addOldBritishMoney money1 money2
let (&-) money1 money2 = subtractOldBritishMoney money1 money2

// 3.2 with record

type OldBritishMoney = {
    pence : int
    shilling : int
    pound : int
}

// The body of the function is actually the same as the body for the triple above, so we could have
// called that directly
let addOldBritishMoneyRec money1 money2 =
    let (pound1, shilling1, pence1) = (money1.pound, money1.shilling, money1.pence)
    let (pound2, shilling2, pence2) = (money2.pound, money2.shilling, money2.pence)
    let (pence, carry) = 
        let sum = pence1 + pence2
        (sum % 12, sum / 12)
    let (shilling, carry) =
        let sum = shilling1 + shilling2 + carry
        (sum % 20, sum / 20)
    let pound = pound1 + pound2 + carry
    {pence = pence; shilling = shilling; pound = pound}

// The body of the function is actually the same as the body for the triple above, so we could have
// called that directly
let subtractOldBritishMoneyRec money1 money2 =
    let (pound1, shilling1, pence1) = (money1.pound, money1.shilling, money1.pence)
    let (pound2, shilling2, pence2) = (money2.pound, money2.shilling, money2.pence)
    let (pence, carry) = 
        let difference = pence1 - pence2
        let willBorrow = difference < 0
        if willBorrow then (difference + 12, 1) else (difference, 0)

    let (shilling, carry) = 
        let difference = shilling1 - shilling2 - carry
        let willBorrow = difference < 0
        if willBorrow then (difference + 20, 1) else (difference, 0)

    let (pound, carry) =
        let difference = pound1 - pound2 - carry
        let willBorrow = difference < 0
        if willBorrow then (difference + 10, 1) else (difference, 0)

    if (carry > 0) then
        failwith "You're out of money!"
    {pence = pence; shilling = shilling; pound = pound}

let (|+) money1 money2 = addOldBritishMoneyRec money1 money2
let (|-) money1 money2 = subtractOldBritishMoneyRec money1 money2
