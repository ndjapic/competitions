# Задатак: A_Question_Marks.pas

```pascal
program A_Question_Marks;
uses
    math;
var
    ntc, tci: int16;
    n, i, a, b, c, d, q: int16;
    s: string;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        readln(s);

        a := 0;
        b := 0;
        c := 0;
        d := 0;
        q := 0;

        for i := 1 to 4*n do
            case s[i] of
                'A': inc(a);
                'B': inc(b);
                'C': inc(c);
                'D': inc(d);
                '?': inc(q);
            end;

        a := min(a, n);
        b := min(b, n);
        c := min(c, n);
        d := min(d, n);

        writeln(a+b+c+d);

    end;
end.

```
