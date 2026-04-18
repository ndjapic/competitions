program D_Take_ABC;
const
    maxn = 200 * 1000 + 1;
var
    n, i, l, r: int32;
    s: array [0 .. maxn] of char;
    iprev, inext: array [0 .. maxn] of int32;

begin
    n := 0;
    repeat
        inc(n);
        read(s[n]);
    until eoln;
    readln;

    s[0] := '*';
    s[n+1] := '*';

    for i := 0 to n do begin
        inext[i] := i+1;
        iprev[i+1] := i;
    end;

    r := inext[0];
    while s[r] <> '*' do
        if (s[r] = 'C') and (s[iprev[r]] = 'B') and (s[iprev[iprev[r]]] = 'A') then begin
            l := iprev[iprev[iprev[r]]];
            r := inext[r];
            iprev[r] := l;
            inext[l] := r;
        end else
            r := inext[r];

    r := inext[0];
    while s[r] <> '*' do begin
        write(s[r]);
        r := inext[r];
    end;
    writeln;
end.
