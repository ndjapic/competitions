{%RunFlags MESSAGES+}
program E_Making_Anti_Palindromes;
uses
    math;
const
    maxn = 200 * 1000;
var
    notc: int16;
    n, m1, m2, i, ans, sumc2: int32;
    ch: char;
    s: array [1 .. maxn] of char;
    c1, c2: array ['a' .. 'z'] of int32;

begin
    readln(notc);
    repeat

        for ch := 'a' to 'z' do begin
            c1[ch] := 0;
            c2[ch] := 0;
        end;

        readln(n);
        for i := 1 to n do begin
            read(s[i]);
            inc(c1[s[i]]);
        end;
        readln;

        if odd(n) then
            ans := -1
        else begin

            for i := 1 to n div 2 do
                if s[i] = s[n+1-i] then inc(c2[s[i]]);

            m1 := 0;
            m2 := 0;
            sumc2 := 0;
            for ch := 'a' to 'z' do begin
                m1 := max(m1, c1[ch]);
                m2 := max(m2, c2[ch]);
                inc(sumc2, c2[ch]);
            end;

            if m1 > n div 2 then
                ans := -1
            else
                ans := max((sumc2+1) div 2, m2);

        end;

        writeln(ans);

        dec(notc);
    until notc = 0;
end.

