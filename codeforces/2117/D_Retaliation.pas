program D_Retaliation;
const
    nn = 200 * 1000;
var
    ntc, tci, n, i, x, y, s: int32;
    ans: boolean;
    a: array [1 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do read(a[i]);
        readln;

        (*
        * x >= 0
        * y >= 0
        * i*x + (n-i+1)*y = a[i]
        * x + n*y = a[1]
        * 2*x + (n-1)*y = a[2]
        * n*x + y = a[n]
        * x + (n-2)*y = a[2] - 2*a[1]
        * *)

        ans := (a[1] + a[n]) mod (n+1) = 0;
        if ans then begin

            s := (a[1] + a[n]) div (n+1);
            ans := ((a[n] - s) mod (n-1) = 0) and ((a[1] - s) mod (n-1) = 0);
            ans := ans and (a[1] >= s) and (a[n] >= s);
            if ans then begin

                x := (a[n] - s) div (n-1);
                y := (a[1] - s) div (n-1);

                i := n;
                while (i > 0) and (int64(n-i+1) * y + int64(i) * x = a[i]) do dec(i);
                ans := i = 0;

            end;

        end;

        if ans then
            writeln('YES')
        else
            writeln('NO');

    end;
end.
