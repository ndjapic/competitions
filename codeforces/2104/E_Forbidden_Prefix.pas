program E_Unpleasant_Strings;
{$MODE DELPHI}
uses
    math;
const
    nn = 1000 * 1000 + 1;
var
    n, m, q, i, j, unseen, d0: int32;
    k: int8;
    ch: char;
    s, t: string;
    o: array ['a' .. 'z'] of int8;
    d: array [0 .. nn] of int32;
    dp: array [0 .. nn] of array [0 .. 25] of int32;

begin
    readln(n, k);
    readln(s);
    readln(q);

    for ch := 'a' to 'z' do begin
        o[ch] := ord(ch) - 97;
        dp[n][o[ch]] := n+1;
    end;

    for i := n downto 1 do begin
        dp[i-1] := dp[i];
        dp[i-1][o[s[i]]] := i;
    end;

    j := n;
    unseen := (1 shl k) - 1;
    d[n+1] := 0;
    for i := n downto 0 do begin

        if i > 0 then
            dec(unseen, (1 shl o[s[i]]) and unseen)
        else
            unseen := 0;

        if unseen = 0 then begin
            d0 := d[j+1] + 1;
            while j >= i do begin
                d[j] := d0;
                dec(j);
            end;
            unseen := (1 shl k) - 1;
        end;

    end;

    while q > 0 do begin
        dec(q);
        readln(t);
        m := length(t);

        i := 0;
        j := 1;
        while (i <= n) and (j <= m) do begin
            i := dp[i][o[t[j]]];
            inc(j);
        end;

        writeln(d[i]);
    end;

end.
