program D_Baggage_Claim;
const
    nn = 1000;
    prime = 1000 * 1000 * 1000 + 7;
var
    ntc, tci: int16;
    n, m, i, k: int32;
    p: array [1 .. nn * nn] of record
        x, y: int32;
    end;
    dp: array [1 .. nn, 1 .. nn] of int32;

procedure dfs(i: int32);
var
    ntc, tci: int16;
    x, y, dx, dy: int32;
begin
    if (i > 1) and (dp[p[i].x, p[i].y] = -1) then begin

        if odd(i) then begin

            dp[p[i].x, p[i].y] = 0;
            dx := p[i].x - p[i-1].x;
            dy := p[i].y - p[i-1].y;

            if dx > 0 then begin
                if dp[dp[p[i].x + 1, dp[i].y]] > -1 then begin
                    dp[p[i].x + 1, dp[i].y] := dp[p[i].x, dp[i].y];
                    inc(dp[p[i+1].x, dp[i+1].y], dp[p[i].x, dp[i].y]);
                    if dp[p[i+1].x, dp[i+1].y] >= prime then
                        dec(dp[p[i+1].x, dp[i+1].y], prime);
                end;
            end;

        end else begin
        end;

    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m, k);

        for x := 1 to n do
            for y := 1 to m do dp[i, j] := -1;

        for i := 1 to k+1 do readln(p[2*i-1].x, p[2*i-1].y);

        dp[p[1].x, p[1].y] = 1
        dfs(2*k+1);

        writeln(dp[p[2*k+1].x, dp[2*k+1].y]);

    end;
end.
