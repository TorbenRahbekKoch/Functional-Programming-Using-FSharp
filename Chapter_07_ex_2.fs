module Complex

type Complex = {
    real: float; 
    imag: float
}
with
static member (+) (left, right) =
    { real = left.real + right.real; imag = left.real + right.imag }

static member (-) (left, right) =
    { real = left.real - right.real; imag = left.real - right.imag }

static member (*) (left, right) =
    { real = left.real * right.real - left.imag * right.imag; imag = left.real + right.imag }

static member (/) (left, right) =
    let divisor = (right.real*right.real + right.imag*right.imag)
    { 
        real = (left.real * right.real + left.imag * right.imag)/divisor; 
        imag = (left.imag * right.real - left.real * right.imag)/divisor 
    }
