program A_Chemistry;
const
    maxn = 100 * 1000;
var
    ntc, tci, n, k, i, c1: int32;
    ch: char;
    c: array ['a' .. 'z'] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k);

        for ch := 'a' to 'z' do c[ch] := 0;

        for i := 1 to n do begin
            read(ch);
            inc(c[ch]);
        end;
        readln;

        c1 := 0;
        for ch := 'a' to 'z' do
            inc(c1, c[ch] mod 2);

        if c1-1 <= k then
            writeln('YES')
        else
            writeln('NO');

    end;
end.
