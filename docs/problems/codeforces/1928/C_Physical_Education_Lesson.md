# Problem: C_Physical_Education_Lesson.pas

```pascal
program C_Physical_Education_Lesson;
var
    ntc, tci, n, k, x, i, t, ans: int32;
    divs: array of int32;

procedure append_div(d: int32);
begin
    inc(t);
    if t = length(divs) then setlength(divs, 2*t);
    divs[t] := d;
end;

procedure make_divs(n: int32);
var
    d, i: int32;
begin
    t := 0;
    d := 1;

    while d*d <= n do begin
        if n mod d = 0 then append_div(d);
        inc(d);
    end;

    i := t;
    if n div divs[t] = divs[t] then dec(i);

    while i > 0 do begin
        append_div(n div divs[i]);
        dec(i);
    end;
end;

begin
    setlength(divs, 1);
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, x);

        ans := 0;

        if not odd(n-x) then begin

            make_divs((n-x) div 2);
            for i := 1 to t do begin
                k := divs[i] + 1;
                if x < k then inc(ans);
            end;

        end;

        if (x > 1) and not odd(n+x-2) then begin

            make_divs((n+x-2) div 2);
            for i := 1 to t do begin
                k := divs[i] + 1;
                if x <= k then inc(ans);
            end;

        end;

        writeln(ans);

    end;
end.

```
