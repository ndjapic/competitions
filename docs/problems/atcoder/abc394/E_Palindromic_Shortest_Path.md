# Problem: E_Palindromic_Shortest_Path.pas

```pascal
program E_Palindromic_Shortest_Path;
const
    nn = 100;
    inf = 200;
var
    n, i, j, i1, j1, i2, j2: int8;
    k1, k2: int16;
    ch: char;
    loop: boolean;
    c: array [1 .. nn] of string;
    a: array [1 .. nn, 1 .. nn] of int16;
    adj: array ['a' .. 'z', 1 .. nn] of array of int8;
    deg: array ['a' .. 'z', 1 .. nn] of int16;

begin
    readln(n);

    for ch := 'a' to 'z' do
        for i := 1 to n do begin
            setlength(adj[ch, i], 1);
            deg[ch, i] := 0;
        end;

    for i := 1 to n do begin
        readln(c[i]);
        for j := 1 to n do begin
            ch := c[i][j];

            if j = i then
                a[i, j] := 0
            else if ch = '-' then
                a[i, j] := inf
            else
                a[i, j] := 1;

            if ch <> '-' then begin
                if length(adj[ch, i]) = deg[ch, i] then
                    setlength(adj[ch, i], 2 * deg[ch, i]);
                adj[ch, i][deg[ch, i]] := j;
                inc(deg[ch, i]);
            end;
        end;
    end;

    loop := true;
    while loop do begin
        loop := false;
        for ch := 'a' to 'z' do
            for i1 := 1 to n do
                for k1 := 0 to deg[ch, i1] -1 do begin
                    j1 := adj[ch, i1][k1];
                    for i2 := 1 to n do
                        if a[j1, i2] < inf then
                            for k2 := 0 to deg[ch, i2] -1 do begin
                                j2 := adj[ch, i2][k2];
                                if a[i1, j2] > a[i2, j1] +2 then begin
                                    a[i2, j2] := a[i2, j1] +2;
                                    loop := true;
                                end;
                            end;
                end;
    end;

    for i := 1 to n do begin
        for j := 1 to n do begin
            if a[i, j] < inf then
                write(a[i, j])
            else
                write('-1');
            if j < n then write(' ');
        end;
        writeln;
    end;
end.

```
