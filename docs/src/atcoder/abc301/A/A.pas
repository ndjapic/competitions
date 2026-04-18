program Overall_Winner;
var
   n, i, nt, na: int8;
   s: array [1 .. 100] of char;

begin
    readln(n);
    nt := 0;
    na := 0;

    for i := 1 to n do begin
        read(s[i]);
        case s[i] of
        'T': inc(nt);
        'A': inc(na);
        end;
    end;

    if nt = na then begin
       if s[n] = 'T' then
          inc(na)
       else
           inc(nt);
    end;

    if nt > na then
       writeln('T')
    else
       writeln('A');
end.

