#include <stdio.h>
#include <stdlib.h>
#include <math.h>

extern double pow(double, double);

#define min(a,b) ((a) < (b) ? (a) : (b))
#define max(a,b) ((a) > (b) ? (a) : (b))


double fac[65];             // fac[n] = n!
double choose2 [49][9][9];  // choose[n][j][k] = Multinomialkoeffizient n! / (j! * k! * (n - j - k)!)
double mopps_[9][33][9];    // Umsetzung von m(p,s,n) aus Satz 4.10
double pawns[2][9][9][9];   


//Berechnet die Werte von fac[i]
void factorial() {
    int i, j;
    for (i=0; i<65; i++) {
        fac[i] = 1.0;
        for (j=2; j<=i; j++) {
            fac[i] *= j;
        }
    }
}

// Berechnet die Werte von choose2[n][k1][k2]
void choose2function(){
    int n, k1, k2;
    for (n = 0; n <= 48; n++){
        for (k1 = 0; k1 <= 8; k1++){
            for (k2 = 0; k2 <= 8; k2++){
                if (n >= k1 + k2){
                    choose2[n][k1][k2] = fac[n] / (fac[k1] * fac[k2] * fac[n - k1 -  k2]);
                } else {
                    choose2[n][k1][k2] = 0;
                }
            }
        }
    }
}

//Berechnet die Werte von mopps[p][s][n]
double opps(int p, int s, int n) {
  double res = 0.0;
  if (p == 0 && s == 0) {
    res = 1;
  } else if (p == 0 || n == 0) {
    res = 0;
  } else {
    res = mopps_[p][s][n - 1];
    for (int i = 0; i <= (s < 4 ? s : 4); ++i) {  
      res += (5 - i) * mopps_[p - 1][s - i][n - 1];
    }
  }
  return res;
}

//Weißt mopps[p][s][n], die in opps(p,s,n) berechneten Werte zu
void moppsfunction(){ 
    double max = 0.0;
    for (int p = 0; p <= 8; ++p) { 
    for (int s = 0; s <= 32; ++s) {
      for (int n = 0; n <= 8; ++n) {
        mopps_[p][s][n] = opps(p, s, n);
      }
    }
  }
}

// Berechnet die innere Summe in Satz 4.12
// dw = 1 <=> weißer Bauer in schwarzer fixierter Spalte
// db = 1 <=> schwarzer Bauer in weißer fixierter Spalte
// ds = ds1 + ds2 = Anzahl an eingeschlossener Felder in fixierten Spalten
double pawnsep (int dw, int db, int ds, int wp, int bp, int opp){ 
    int opp_ = opp - dw - db;                  // Mindestanzahl blockierter nicht-fixierter Spalten
    int s;                                     // Anzahl eingeschlossener Felder
    double res = 0.0;
    for (s = 0; s <= 4 * opp_; s++){           
        if(bp + dw >= opp && wp + db >= opp){  // Damit keine negative (also nichtdefinierte) Komponente von choose2 aufgerufen wird
            res += mopps_[opp_][s][6] * choose2[44 - opp - opp_ - ds - s][wp + db - opp][bp + dw - opp];
        }
    }
    return res;
}


void pawnsfunction(){
    int s, wp, bp, opp;
    int ds1, ds2;
    double res;

    // Fall: Kein Enpassant -> fixp = 0
    // Berechnet obere Schranke aus Satz 4.11
    for (wp = 0; wp <= 8; wp++){
        for (bp = 0; bp <= 8; bp++){
            for (opp = 0; opp <= 8; opp++){
                if (opp > min(wp, bp)){ // Es darf nicht mehr blockierte Spalten als Bauern einer Seite geben
                    pawns[0][wp][bp][opp] = 0;
                } else {
                    res = 0.0;
                    for (s = 0; s <= 4 * opp; s++){ 
                        res += mopps_[opp][s][8] * choose2[48 - 2 * opp - s][wp - opp][bp - opp];
                    }
                    pawns[0][wp][bp][opp] = res;
                }
            }
        }
    }

    // Fall: Enpassant -> fixp = 1
    // Berechnet obere Schranke aus Satz 4.12
    for (wp = 0; wp <= 8; wp++){
        for (bp = 0; bp <= 8; bp++){
            for (opp = 0; opp <= 8; opp++){
                res = 0;
                if (opp > min(wp + 1, bp +1)){ // Es darf nicht mehr blockierte Spalten als Bauern einer Seite geben
                    pawns[1][wp][bp][opp] = 0;
                } else {
                    res = pawnsep(0, 0, 0, wp, bp, opp);
                    for (ds1 = 0; ds1 <= 2 && opp > 1; ds1++){
                        for (ds2 = 0; ds2 <= 1; ds2++) {
                            res += pawnsep(1, 1, ds1 + ds2, wp, bp, opp);
                        }
                    }
                    for (ds1 = 0; ds1 <= 2 && opp > 0; ds1++){
                        res += pawnsep(1, 0, ds1, wp, bp, opp);
                    }
                    for (ds2 = 0; ds2 <= 1 && opp > 0; ds2++){
                        res += pawnsep(0, 1, ds2, wp, bp, opp);
                    }
                    pawns[1][wp][bp][opp] = res;
                }
            }
        }
    }
}

// Berechnet eine obere Schranke  (bis auf Multiplikatoren) für die Anzahl an Schachpositionen, bei denen Weiß am Zug ist
// in Abhängigkeit von fwr, fbr und fixp
double count (int fwr, int fbr, int fixp){
    double summe = 0.0;
    double h = 0.0;                 // Hilfsvariable, damit der Code nicht zu weit nach rechts geht & gedruckt werden kann
    int np = 8 - fixp;              // Maximal mögliche nicht-fixierte Bauern pro Seite
    int fixwk = (fwr != 0 ? 1 : 0); // Es existiert ein fixierter König <=> Mind. 1 Fixierter Turm existiert
    int fixbk = (fbr != 0 ? 1 : 0);
    int uwr, wk, wq, wnp1, wr, wb, wnp2, wnp3, wn, wnp4, wproms, wp, wpx;
    int ubr, bk, bq, bnp1, br, bb, bnp2, bnp3, bn, bnp4, bproms, bp, bpx;
    int wpcs, bpcs, caps, maxuwp, maxubp, minopp, space;

    // wprod und bprod sind Produkte von Fakultäten und dienen zur Berechnung eines Multinomialkoeffizienten
    double wprod, bprod;

    // Anzahl der weißen Figuren
    uwr = 2 - fwr;              // Da es 2 Türme zum Start gibt, müssen mind. 2 - fwr Türme nicht-fixiert sein.
    wk = floor(uwr / 2);        // Anzahl nicht-fixierter Könige 


    // Die Verringerung der Variable np zu wnp1 zu Variablen wie wnp4 hilft Figurenkombinationen herauszufiltern, 
    // für die zu viele Promotionen nötig wären.

    for (wq = 0; wq <= 1 + np; wq++){                                           // wq = Anzahl weißer Damen
        wnp1 = np - max(wq - 1, 0);
        for (wr = 0; wr <= uwr + wnp1; wr++){                                   // wr = Anzahl weißer nicht-fixierter Türme
            wnp2 = wnp1 - max(wr - uwr, 0);
            for(wb = 0; wb <= 2 + wnp2; wb++){                                  // wb = Anzahl weißer Läufer 
                wnp3 = wnp2 - max(wb - 2, 0);
                for (wn = 0; wn <= 2 + wnp3; wn++){                             // wn = Anzahl weißer Springer
                    wnp4 = wnp3 - max(wn - 2, 0);

                    wproms = np - wnp4;                                         // wproms = Anzahl weißer Promotionen

                    wprod = fac[wk] * fac[wq] * fac[wr] * fac[wb] * fac[wn];    // Dient zur Berechnung eines Multinomialkoeff.

                    for (wp = 0; wp <= wnp4; wp++){                             // wp = Anzahl nicht-fixierter weißer Bauern 

                        wpx = np - wp - wproms;                                 // wpx = Anzahl geschlagener weißer Bauern
                        
                        wpcs = wk + wq + wr + wb + wn;                          // wpcs = Anzahl weißer Pieces mit nichtfix König 
                            
                        // Anzahl der schwarzen Figuren
                        ubr = 2 - fbr; // Mindestanzahl nicht-fixierter schwarzer Türme
                        bk = floor(ubr / 2); // Anzahl nicht-fixierter schwarzer Könige

                        for (bq = 0; bq <= 1 + np; bq++){                       // bq = Anzahl schwarzer Damen
                            bnp1 = np - max(bq - 1, 0);
                            for (br = 0; br <= ubr + bnp1; br++){               // br = Anzahl schwarzer nicht-fixierter Türme
                                bnp2 = bnp1 - max(br - ubr, 0);
                                for(bb = 0; bb <= 2 + bnp2; bb++){              // bb = Anzahl schwarzer Läufer 
                                    bnp3 = bnp2 - max(bb - 2, 0);
                                    for (bn = 0; bn <= 2 + bnp3; bn++){         // bn = Anzahl schwarzer Springer
                                        bnp4 = bnp3 - max(bn - 2, 0);

                                        bproms = np - bnp4;                     // bproms = Anzahl schwarzer Promotionen 

                                        bprod = fac[bk] * fac[bq] * fac[br] * fac[bb] * fac[bn];

                                        for (bp = 0; bp <= bnp4; bp++){         // bp = Anzahl nicht-fixierter schwarzer Bauern

                                            bpx = np - bp - bproms;             // bpx = Anzahl geschlagener schwarzer Bauern

                                            bpcs = bk + bq + br + bb + bn;      // bpcs = Anzahl schwarzer Pieces & nicht-fix. König

                                            // Ist der weiße König fixiert ist fixwk = 1,
                                            // sonst ist der nicht-fixierte weiße König, in wpcs enthalten. Für Schwarz analog
                                            // caps = 32 - alle noch vorhandenen Figuren
                                            caps = 32 - 2 * fixp - fixwk - fixbk - fwr - fbr - wp - bp - wpcs - bpcs;

                                            maxuwp = bpx + caps - wproms;       // Maximalanzahl weißer Bauern in freien Spalten
                                            maxubp = wpx + caps - bproms;       // Maximalanzahl schwarzer Bauern in freien Spalten
                                            h = 0.0;
                                            // Testet die Bedingung aus Lemma 4.7
                                            if (maxuwp < 0 || maxubp < 0 ){
                                                continue;
                                            } else {
                                                // Mindestanzahl blockierter Spalten
                                                minopp = max(0, fixp + wp - maxuwp);    
                                                space = 64 - 4 * fixp - fixwk - fixbk - fwr - fbr - wp - bp;
                                                h += pawns[fixp][wp][bp][minopp]; 
                                                h *= fac[space]/ (fac[space - wpcs - bpcs] * wprod * bprod);
                                                summe += h;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    return summe;
}

int main (){

    //Initialisierung der Arrays
    factorial();
    choose2function();
    moppsfunction();
    pawnsfunction();
    int fwr, fbr, fixp;
    double res = 0.0;
    double summe = 0.0;
    double multFWR, multEP, multFBR;

    // Alle möglichen Werte für fwr, fbr und fixp durchlaufen
    for (fwr = 0; fwr <= 2; fwr++){

        multFWR = (fwr == 1 ? 2 : 1); //multFWR
        
        for (fbr = 0; fbr <= 2; fbr++){

            multFBR = (fbr == 1 ? 2 : 1); //multFBR


            for (fixp = 0; fixp <= 1; fixp++){

                multEP = (fixp == 1 ? 14 : 1); //multEP

                //Multiplikatoren heranmultiplizieren
                res = multEP * multFWR * multFBR * count(fwr, fbr, fixp);
                printf("fwr = %d\tfbr = %d\tfixp = %d\tgerundet: %e\texakt: %1.0f\n", fwr, fbr, fixp, res, res);
                summe += res;
            }
        }
    }

    // Anzahl wird mit 2 multipliziert um die Schachpositionen in denen Schwarz am Zug ist zu berücksichtigen
    printf("Totale Summe: %1.0f \n", 2 * summe);
    return 1;
}
