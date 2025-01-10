program Number_Theoretic_Transform;
{$mode delphi}
uses
    math;
const
    prime = 7*17 * 8192 * 1024 + 1;

type
    TPolynomial = array of int32;
    TNTT = class
        var
            root, invr: array [0 .. 23] of int32;
        function pow(b, e: int32): int32;
        constructor create;
        procedure transform(var a, b: TPolynomial; l: int32; e: int8; invert: boolean);
        procedure mul(var p: TPolynomial; p1, p2: TPolynomial);
    end;

function TNTT.pow(b, e: int32): int32;
begin
    if e = 0 then
        result := 1
    else if odd(e) then
        result := int64(b) * pow(b, e-1) mod prime
    else
        result := pow(int64(b) * b mod prime, e div 2);
end;

constructor TNTT.create;
var
    b, e: int8;
begin
    b := 2;
    while
        (pow(b, (prime-1) div 2) = 1) or
        (pow(b, (prime-1) div 7) = 1) or
        (pow(b, (prime-1) div 17) = 1) do inc(b);

    root[23] := pow(b, (prime - 1) shr 23);
    invr[23] := pow(root[23], prime - 2);
    for e := 22 downto 0 do begin
        root[e] := pow(root[e+1], 2);
        invr[e] := pow(invr[e+1], 2);
    end;
end;

procedure TNTT.transform(var a, b: TPolynomial; l: int32; e: int8; invert: boolean);
var
    wn, w, h, m, i: int32;
begin
    if e > 0 then begin

        h := int32(1) shl (e-1);
        m := l+h;
        for i := 0 to h-1 do begin
            b[l+i] := a[l+2*i];
            b[m+i] := a[l+2*i+1];
        end;
        transform(b, a, l, e-1, invert);
        transform(b, a, m, e-1, invert);

        w := 1;
        if invert then
            wn := invr[e]
        else
            wn := root[e];

        for i := 0 to h-1 do begin
            b[m+i] := int64(b[m+i]) * w mod prime;
            a[l+i] := b[l+i] + b[m+i];
            a[m+i] := b[l+i] - b[m+i];
            if a[l+i] >= prime then dec(a[l+i], prime);
            if a[m+i] < 0 then inc(a[m+i], prime);
            w := int64(w) * wn mod prime;
        end;

        if invert then
            for i := l to m+h-1 do begin
                if odd(a[i]) then inc(a[i], prime);
                a[i] := a[i] div 2;
            end;

    end;
end;

procedure TNTT.mul(var p: TPolynomial; p1, p2: TPolynomial);
var
    n1, n2, n, i: int32;
    e: int8;
    q: TPolynomial;
begin
    n1 := length(p1);
    n2 := length(p2);

    n := 1;
    e := 0;
    while n < n1+n2 do begin
        inc(n, n);
        inc(e);
    end;

    setlength(p, n);
    setlength(q, n);
    setlength(p1, n);
    setlength(p2, n);
    for i := n1 to n-1 do p1[i] := 0;
    for i := n2 to n-1 do p2[i] := 0;

    transform(p1, q, 0, e, false);
    transform(p2, q, 0, e, false);
    for i := 0 to n-1 do p[i] := int64(p1[i]) * p2[i] mod prime;
    transform(p, q, 0, e, true);

    while p[n-1] = 0 do dec(n);
    setlength(p, n);
end;

var
    p1, p2, p3, p: TPolynomial;
    ntt: TNTT;

procedure writepoly(p: TPolynomial);
var
    i: int32;
begin
    for i := 0 to length(p) - 1 do begin
        if p[i] > prime div 2 then
            write(' ', p[i] - prime)
        else
            write(' +', p[i]);
        if i > 0 then write('*x');
        if i > 1 then write('^', i);
    end;
    writeln;
end;

begin
    ntt := TNTT.create;

    setlength(p1, 2);
    setlength(p2, 2);
    setlength(p3, 3);

    p1[1] := 1;
    p1[0] := -2 +prime;

    p2[1] := 1;
    p2[0] := 2;

    p3[2] := 1;
    p3[1] := 0;
    p3[0] := 4;

    ntt.mul(p, p1, p2);
    ntt.mul(p, p, p3);

    write('p1 ='); writepoly(p1);
    write('p2 ='); writepoly(p2);
    write('p3 ='); writepoly(p3);
    write('p ='); writepoly(p);

    ntt.free;
end.

(*
p1 = -2 +1*x
p2 = +2 +1*x
p3 = +4 +0*x +1*x^2
p = -16 +0*x +0*x^2 +0*x^3 +1*x^4


------------------
(program exited with code: 0)
Press return to continue
*)
