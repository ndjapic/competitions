program B_A_B_C;
{$MODE DELPHI}
var
    n, j, d: int8;
    ans: int32;
    s: string;

begin
    readln(s);
    n := length(s);

    ans := 0;
    for d := 1 to (n-1) div 2 do
        for j := 1+d to n-d do
            if (s[j-d] = 'A') and (s[j] = 'B') and (s[j+d] = 'C') then
                inc(ans);

    writeln(ans);
end.
