noah([X,Y],[[X,Y]]).
noah([X,Y|Ys],[[X,Y]|Zs]) :- noah(Ys,Zs).
