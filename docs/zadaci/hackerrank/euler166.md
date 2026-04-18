# Задатак: euler166.pas

```pascal
program Criss_Cross;
var
    n, s,
    a11, a12, a13, a14,
    a21, a22, a23, a24,
    a31, a32, a33, a34,
    a41, a42, a43, a44: int8;
    ans: int32;

function ok(x: int8): boolean;
begin
    ok := (0 <= x) and (x <= n);
end;

begin
    readln(n);
    ans := 0;

    for a11 := 0 to n do begin
        for a12 := 0 to n do begin
            for a13 := 0 to n do begin
                for a14 := 0 to n do begin

                    s := a11 + a12 + a13 + a14;
                    for a22 := 0 to n do begin
                        for a32 := 0 to n do begin
                            a42 := s - a12 - a22 - a32;
                            if ok(a42) then begin
                                for a23 := 0 to n do begin
                                    a41 := s - a14 - a23 - a32;
                                    if ok(a41) then begin
                                        for a33 := 0 to n do begin
                                            a43 := s - a13 - a23 - a33;
                                            a44 := s - a11 - a22 - a33;
                                            if ok(a43) and ok(a44) and (a41+a42+a43+a44 = s) then begin

                                                for a21 := 0 to n do begin
                                                    a24 := s - a21 - a22 - a23;
                                                    a31 := s - a11 - a21 - a41;
                                                    a34 := s - a14 - a24 - a44;
                                                    if ok(a24) and ok(a31) and ok(a34) and (a31+a32+a33+a34 = s) then inc(ans);
                                                end;

                                            end;
                                        end;
                                    end;
                                end;
                            end;
                        end;
                    end;

                end;
            end;
        end;
    end;

    writeln(ans);
end.

```
