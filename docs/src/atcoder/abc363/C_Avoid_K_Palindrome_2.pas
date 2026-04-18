program C_Avoid_K_Palindrome_2;
{$mode objfpc}{$H+}{$J-}
const
    nn = 10;
var
    n, k, i, j: int8;
    ans: int32;
    ch: char;
    s: string;
    c: array ['a' .. 'z'] of int8;

procedure dfs(i: int8);
var
    ch: char;
    found: boolean;
begin
    if i <= n then begin

        for ch := 'a' to 'z' do
            if c[ch] > 0 then begin
                dec(c[ch]);
                s[i] := ch;
                dfs(i+1);
                inc(c[ch]);
            end;

    end else begin

        i := 0;
        found := false;
        while not found and (i <= n-k) do begin
            j := 1;
            while (j < k+1-j) and (s[i+j] = s[i+k+1-j]) do inc(j);
            found := j >= k+1-j;
            inc(i);
        end;

        if not found then inc(ans);

    end;
end;

begin
    readln(n, k);
    readln(s);

    for ch := 'a' to 'z' do c[ch] := 0;
    for i := 1 to n do inc(c[s[i]]);

    ans := 0;
    dfs(1);
    writeln(ans);
end.
