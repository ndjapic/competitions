program odd_bit_subsequence;
const
    maxn = 100 * 1000;
    prime = 1000 * 1000 * 1000 + 7;
var
    ntc, p: int8;
    n, i, ai: int32;
    parity: array [0 .. 32767] of int8;
    dp: array [0 .. 1, 0 .. maxn] of int64;
 
begin
    parity[0] := 0;
    for i := 0 to 16383 do begin
        parity[2*i] := parity[i];
        parity[2*i+1] := 1 - parity[i];
    end;
 
    readln(ntc);
    repeat
 
        readln(n);
        dp[0, 0] := 1;
        dp[1, 0] := 0;
 
        for i := 1 to n do begin
 
            read(ai);
            p := parity[ai shr 15] xor parity[ai and 32767];
            dp[p, i] := dp[0, i-1] + dp[1, i-1];
            dp[1-p, i] := dp[1, i-1];
            if dp[p, i] >= prime then dec(dp[p, i], prime);
 
        end;
        readln;
 
        writeln(dp[1, n]);
 
        dec(ntc);
    until ntc = 0;
end.
