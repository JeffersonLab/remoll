#include <stdio.h>
#include <sstream>
#include <vector>
#include "TGraph.h"
#include "TMultiGraph.h"
#include "TCanvas.h"
#include "TPad.h"
#include "TTree.h"
#include "TFile.h"
#include "TImage.h"
#include <limits>
#include <fstream>
#include "remolltypes.hh"

double* get99Area(double, double, double, double);

//TODO better checking of the bottom
double epsilon = 0.0001;


int z_pos = 0; //default starting position is z=0
//int i = 0;
TGraph* g2;
TMultiGraph *mg = new TMultiGraph();
TGraph* hull;
const int maxv = 100; //maximum number of points on the hull
const double smoothFactor = 0; //deviation from cos^2 (theta) = -1 that is still a straight line 
double minArea = 50; //minimum area unit considered. smaller = more points, larger = more meaningful points (but less overall)
//TODO edit to make relative
double criticalPoints = 0.00000000; //difference in density to register a point. smaller = more points (but noisier)
double maxAreaFactor = 16;
double maxArea = maxAreaFactor * minArea; //do not ignore any area larger than this, regardless of density
int startK = 10; //starting number of points to look at. Higher is a smoother hull
//make these density units relative
double cut = 1; //1.0; //if both squares have a density higher than this, ignore both
double densityCut = 7; //if the square has a density lower than this, ignore it as noise
int stepK = 10;
int pointLimit = 10000; //max number of points to consider for the hull

std::vector<double> particlesX;
std::vector<double> particlesY;

int goodParticles;

int countTextFile(int thisZ, char* filename) {
    int tempGoodParticles = 0;
    std::vector < remollEventParticle_t > *particle = 0;
    TFile f(filename);
    TTree *tree = (TTree*)f.Get("T");
    //tree->Print();
    //TODO reading data into envelopes downstream could be sped up
    //by storing the data by Z instead of by hit
    tree->SetBranchAddress("part", &particle); 
    for (int i = 0; i < tree->GetEntries(); i++)
    {
        tree->GetEntry(i);
        for (size_t j = 0; j < particle->size(); j++)
        {
            std::vector < double > *z = &(particle->at(j).tjz);
            for (size_t k = 0; k < z->size(); k++)
            {
                if (z->at(k) == thisZ)
                {
                    tempGoodParticles++;
                    break;
                }
            }
        }
    }
    std::cout << "Good Particles: " << tempGoodParticles << std::endl;
    return tempGoodParticles;	
}



void readTextFile(int thisZ, char* filename){
    std::vector < remollEventParticle_t > *particle = 0;
    TFile f(filename);
    TTree *tree = (TTree*)f.Get("T");
    tree->SetBranchAddress("part", &particle); 
    std::cout << "check size tree " << tree->GetEntries() << " " << __LINE__ << std::endl;
    for (int i = 0; i < tree->GetEntries(); i++)
    {
        tree->GetEntry(i);
        for (size_t j = 0; j < particle->size(); j++)
        {
            std::vector < double > *x = &particle->at(j).tjx;
            std::vector < double > *y = &particle->at(j).tjy;
            std::vector < double > *z = &particle->at(j).tjz; 
            //std::cout << "check size x " << x->size() << " " << __LINE__ << std::endl;
            //std::cout << "check size y " << y->size() << " " << __LINE__ << std::endl;
            //std::cout << "check size z " << z->size() << " " << __LINE__ << std::endl;
            for (size_t k = 0; k < z->size(); k++)
            {
                if (z->at(k) == thisZ)
                {
                    particlesX.push_back(x->at(k));
                    particlesY.push_back(y->at(k));
                    break;
                }
            }
        }
    }
}


int countPointsInBounds(double rightBound, double leftBound, double upperBound, double lowerBound)
{
    int count = 0;
    for (unsigned int i = 0; i < goodParticles; i++)
    {
        if (particlesX[i] < rightBound && particlesX[i] > leftBound && particlesY[i] < upperBound && particlesY[i] > lowerBound)
            count++;
    }
    return count;
}
double checkQuadrant(double rightBound, double leftBound, double upperBound, double lowerBound, int pointLimit)
{
    double xDelta = rightBound - leftBound;
    double yDelta = upperBound - lowerBound;
    double area = xDelta * yDelta;

    //std::cout << "points: " << countPointsInBounds(rightBound, leftBound, upperBound, lowerBound) << std::endl;
    //std::cout << "area:" << area << std::endl;
    //std::cout << "right:" << rightBound << std::endl;
    //std::cout << "left:" << leftBound << std::endl;
    //std::cout << "upper:" << upperBound << std::endl;
    //std::cout << "lower:" << lowerBound << std::endl;
    int nPoints = countPointsInBounds(rightBound, leftBound, upperBound, lowerBound);
    double density = nPoints/area;
    if (g2->GetN() >= pointLimit || (nPoints == 0 || (area <= maxArea && nPoints <= densityCut)))
    {
        TGraph *g3 = new TGraph();
        g3->SetPoint(0, rightBound, upperBound);
        g3->SetPoint(1, rightBound, lowerBound);
        g3->SetPoint(2, leftBound, lowerBound);
        g3->SetPoint(3, leftBound, upperBound);
        int densityColor = (int)(density);
        if (densityColor <= .1) densityColor =2;
        if (densityColor >= 1) densityColor =99;
        g3->SetFillColor(densityColor);
        mg->Add(g3);
        return -1;
    }
    else //enough points to not be noise
    { 	//but where are those points?
        //std::cout << "recur" << std::endl;
        if (16*area <= minArea) return density;
        double d[4];
        d[0] = checkQuadrant(rightBound - (xDelta/2), leftBound, upperBound - (yDelta/2), lowerBound, pointLimit); //bot  left	
        d[1] = checkQuadrant(rightBound, rightBound - (xDelta/2), upperBound - (yDelta/2), lowerBound, pointLimit); //bot right	
        d[2] = checkQuadrant(rightBound - (xDelta/2), leftBound, upperBound, upperBound - (yDelta/2), pointLimit); //top left	
        d[3] = checkQuadrant(rightBound, rightBound - (xDelta/2), upperBound, upperBound - (yDelta/2), pointLimit); //top right	
        if (area <= minArea)
        {
            /*std::cout << "Point checking " << std::endl;
            for (int i = 0; i <4 ; i ++)
                std::cout << i << ": " << d[i] << std::endl;
            */
            //TODO edit to make relative instead of absolute
            // across bot
            if ((d[0] < 0 || d[1] < 0) && abs(d[0] - d[1]) > criticalPoints)
            {
                g2->SetPoint(g2->GetN(), rightBound - (xDelta/2), lowerBound + (yDelta/4));
            }
            // across left
            if ((d[0] < 0 || d[2] < 0) && abs(d[0] - d[2]) > criticalPoints)
            {
                g2->SetPoint(g2->GetN(), leftBound + (xDelta/4), lowerBound + (yDelta/2));
            }
            // across diags
            if ( ( (d[0] < 0 || d[3] < 0) && abs(d[0] - d[3]) > criticalPoints) 
                    || ((d[1] < 0 || d[2] < 0) && abs(d[1] - d[2]) > criticalPoints))
            {
                g2->SetPoint(g2->GetN(), rightBound - (xDelta/2), lowerBound + (yDelta/2));
            }
            // across right
            if ((d[1] < 0 || d[3] < 0) && abs(d[1] - d[3]) > criticalPoints)
            {
                g2->SetPoint(g2->GetN(), rightBound - (xDelta/4), lowerBound + (yDelta/2));
            }
            // across top
            if ((d[2] < 0 || d[3] < 0) && abs(d[2] - d[3]) > criticalPoints)
            {
                g2->SetPoint(g2->GetN(), rightBound - (xDelta/2), upperBound - (yDelta/4));
            }
            //std::cout << g2->GetN() << std::endl;
        }
        if (d[0] == -1 && d[1] == -1 && d[2] == -1 && d[3] == -1)
        {
            return -1;
        }
        return density;
    }
    //std::cout << "Density: " << density << std::endl;
}

bool isCounterClockwise(double p1x, double p1y, double p2x, double p2y, double p3x, double p3y )
{
    return ((p2x - p1x)*(p3y-p1y) - (p2y-p1y)*(p3x-p1x)) < 0;
}

int countPointsUnderLine(double sX, double sY, double eX, double eY)
{
    int count = 0;

    int sign = -1;
    if (sX > eX)
    {
        sign *= -1;   
        double tX = sX, tY = sY;
        sX = eX; sY = eY;
        eX = tX; eY = tY;
    }

    double slope = ((eY-sY) / (eX-sX));


    for (unsigned int i = 0; i < goodParticles; i++)
    {
        if (particlesX[i] >= sX  && particlesX[i] <= eX && particlesY[i] <= (sY + slope*(particlesX[i]-sX)))
            count++;
        //std::cout << "X: " << particlesX[i] << endl << "Y: :" << particlesY[i] << endl;
    }
    return sign*count;
}

double getPointsEnclosed(TGraph* graph)
{
    double points = 0;
    for (int i = 0; i < graph->GetN()-1; i++)
    {
        double sX, sY, eX, eY;
        graph->GetPoint(i, sX, sY);
        graph->GetPoint(i+1, eX, eY);

        //stats[0] += (eX - sX) *((sY + eY)/2);//trapezoidal area      
        //stats[1] 
        points += countPointsUnderLine(sX, sY, eX, eY); //count
    }
    return points;
}
double distX, distY;
Bool_t distanceComparator(const TGraph* gr, Int_t i, Int_t j)
{
    double fX, fY, sX, sY;
    gr->GetPoint(i, fX, fY);
    gr->GetPoint(j, sX, sY);
    return sqrt(pow(distX-fX, 2) + pow(distY - fY, 2)) > sqrt(pow(distX-sX, 2) + pow(distY - sY, 2)); 
}


TGraph* getNearestPoints(TGraph* data, double cX, double cY, int k)
{
    distX = cX;
    distY = cY;
    k = (k < data->GetN())? k : data->GetN();
    TGraph* nearest = new TGraph(*data);
    nearest->Sort(&distanceComparator);

    while (nearest->GetN() > k)
    {
        nearest->RemovePoint(nearest->GetN()-1);
    }
    return nearest;
}
double dist(double x1, double y1, double x2, double y2)
{
    return sqrt(pow(x1-x2, 2) + pow(y1-y2, 2));
}
bool IntersectsQ(TGraph* g1, int pid1, TGraph* g2, int pid2, TGraph* g3, int pid3, TGraph* g4, int pid4)
{
    double p1x, p1y, p2x, p2y, p3x, p3y, p4x, p4y;
    g1->GetPoint(pid1, p1x, p1y);
    g2->GetPoint(pid2, p2x, p2y);
    g3->GetPoint(pid3, p3x, p3y);
    g4->GetPoint(pid4, p4x, p4y);     

    double s1x = p2x-p1x; double s1y = p2y-p1y;
    double s2x = p4x-p3x; double s2y = p4y-p3y;

    double snum = (-s1y * (p1x - p3x) + s1x * (p1y - p3y));
    double tnum = (s2x * (p1y - p3y) - s2y * (p1x - p3x));
    double den = (-s2x * s1y + s1x * s2y);

    //FIXME
    double s = snum/den;
    double t = tnum/den;
    double ix, iy;

    /*std::cout << "Collision?"<< std::endl;
      std::cout << "p1x: " << p1x <<std::endl;
      std::cout << "p1y: " << p1y <<std::endl;
      std::cout << "p2x: " << p2x <<std::endl;
      std::cout << "p2y: " << p2y <<std::endl;
      std::cout << "p3x: " << p3x <<std::endl;
      std::cout << "p3y: " << p3y <<std::endl;
      std::cout << "p4x: " << p4x <<std::endl;
      std::cout << "p4y: " << p4y <<std::endl;

      std::cout << "snum: " << snum << std::endl;
      std::cout << "s: " << s << std::endl;
      std::cout << "tnum: " << tnum << std::endl;
      std::cout << "t: " << t << std::endl;
      std::cout << "den: " << den << std::endl;
        */
    if (abs(den) <= epsilon)
    {
        if ( abs(dist(p1x, p1y, p3x, p3y) + dist(p3x, p3y, p2x, p2y) - dist(p1x, p1y, p2x, p2y)) <= epsilon)
            return true;
        if ( abs(dist(p1x, p1y, p4x, p4y) + dist(p4x, p4y, p2x, p2y) - dist(p1x, p1y, p2x, p2y)) <= epsilon)
            return true;
        
       return false; 
    }   

    if (s >= 0 && s <= 1 && t >= 0 && t <= 1)
    {
        //Collision detected
        ix = p1x + (t * s1x);
        iy = p1y + (t * s1y);
        /*std::cout << "Collision found at " << std::endl;
          std::cout << "ix: " << ix << std::endl;
          std::cout << "iy: " << iy << std::endl;
          */
        //if (t == 0) return false; //line segments are allow to connect to form a hull
        return true;
    }
    return false;
}

const double pi = 3.1415926535897;
double getAngle(double x1, double y1, double x2, double y2)
{
    double res = - atan2(y2, x2) + atan2(y1, x1);
    return ((res < 0) ? res + 2*pi : res);
}

double cX, cY;
double prevX, prevY;
double getAngle(int x, int y)
{
    return getAngle(prevX-cX, prevY-cY, x-cX, y-cY);
    //return getAngle(x1, y1, x2, y2);
}


Bool_t AngleComparator(const TGraph* gr, Int_t i, Int_t j)
{
    double tX, tY, nX, nY;
    gr->GetPoint(i, tX, tY);
    gr->GetPoint(j, nX, nY);
    return getAngle(prevX-cX, prevY-cY, tX-cX, tY-cY) > getAngle(prevX-cX, prevY-cY, nX-cX, nY-cY);   
    //return getAngle(prevX, prevY, tX, tY) > getAngle(prevX, prevY, nX, nY);   
}

void RemovePoint(TGraph* data, double cX, double cY)
{
    for (int i = 0; i < data->GetN(); i++)
    {
        double tX, tY;
        data->GetPoint(i, tX, tY);
        if (cX == tX && cY == tY)
        {
            data->RemovePoint(i);
            return;
        }
    }
    std::cerr << "Unable to find point placed?" << std::endl;
    exit(1);
}

TGraph* orderPoints(TGraph* pointList, int k)
{
    std::cout << "Ordering with k=" << k <<  std::endl;
    TGraph *kNearestPoints;
    hull = new TGraph();
    TGraph *data = new TGraph(*pointList);
    k = (k< 3)? 3 : k; //k must be >= 3
    if (data->GetN() < 3) return hull;
    if (data->GetN() == 3) {hull = data; return hull;}

    if (k > data->GetN()) return hull;

    //first point is the lowest
    //data->Sort(TGraph::CompareY);
    double xMin, yMin, xMax, yMax;
    data->ComputeRange(xMin, yMin, xMax, yMax);
    cX = (xMax+xMin)/2;
    cY = (yMax+yMin)/2;
    data->Sort(&AngleComparator, kFALSE); 
    std::cout << "First Point: " << cX << ", " << cY << std::endl;
    double tX, tY;
    data->GetPoint(0,  tX, tY);
    hull->SetPoint(0, tX, tY);
    data->RemovePoint(0);
    prevX = tX-1; prevY = tY; 
    cX = tX; cY = tY;
    double step = 2;
    
    std::cout << "First Point: " << cX << ", " << cY << std::endl;
        
    while(((cX != tX || cY != tY) || step ==2) && (data->GetN() > 0))
    {
        if (step == 5) //add the first point back in to be able to close the hull
        {
            data->SetPoint(data->GetN(), tX, tY);
        }
        if (hull->GetN() > maxv)
        {
            std::cout << "Too many vertices. Generating smoother hull..." << std::endl;
            return orderPoints(pointList, k+stepK);
        }

        //std::cout << "check 1" << std::endl;
        //get and sort neighbors descending by right hand turn
        kNearestPoints = getNearestPoints(data, cX, cY, k);
        //std::cout << "check 2 " << kNearestPoints->GetN() << std::endl;
        kNearestPoints->Sort(&AngleComparator, kFALSE); 

        /*if (step >= 500) //FIXME
          {
        //kNearestPoints->Draw("AL");

        for (int it = 0; it < kNearestPoints->GetN(); it++)
        {
        double bX, bY;
        kNearestPoints->GetPoint(it, bX, bY);
        std::cout << "bp " << it << ": " << bX << ", " << bY << std::endl;
        std::cout << "@ " << getAngle(bX, bY) << std::endl;

        }
        TGraph * test = new TGraph();
        test->SetPoint(0, -1, 0);
        test->SetPoint(1, 1.5, 0);

        test->SetPoint(2, 1, 0);
        test->SetPoint(3, 2, 0);

        std::cout << "Intersect Test" << IntersectsQ(test, 2, test, 3, test, 0, test, 1) << std::endl;
        std::cout << "angle test" << getAngle(0, 1, -1, 0) << std::endl;
        std::cout << "angle test" << getAngle(0, 1, 0, 1) << std::endl;
        std::cout << "angle test" << getAngle(0, 1, 1, 0) << std::endl;
        std::cout << "angle test" << getAngle(0, 1, 0, -1) << std::endl;

        return hull; 
        }*/

        bool intersects = true;
        int i = 0;;
        for (i = 0; i < kNearestPoints->GetN(); i++)
        {
            double pX, pY;
            kNearestPoints->GetPoint(i, pX, pY);
            //std::cout << pX << ", " << pY << "?" << std::endl;
            int lastPoint;
            if (pX == tX && pY == tY)
                lastPoint = 1;
            else
                lastPoint = 0;

            int j = 3;
            intersects = false;
            while (!intersects && j <= hull->GetN() - lastPoint)
            {
                  /*std::cout << "hull " << step-2 << std::endl;
                  std::cout << "kNear " << i << std::endl;
                  std::cout << "hull " << step-1-j << std::endl;
                  std::cout << "hull " << step-j << std::endl;
                  */intersects = IntersectsQ(hull, step-2, kNearestPoints, i, hull, step-1-j, hull, step-j);
                //std::cout << "Intersects: " << intersects << std::endl;
                j++;
            }
            if (!intersects)
                break;
        }
        if (intersects){ //all intersections. Must increase k
            std::cout << "Too many intersections." << std::endl;   
           /* std::cout << "Hull: " << hull->GetN() << std::endl;
            for (int i = 0; i < hull->GetN(); i++)
            {   
                double printX, printY;
                hull->GetPoint(i, printX, printY);
                std::cout << "\t" << printX << ", " << printY << std::endl;
            }*/
            
            //return hull;
            return orderPoints(pointList, k+stepK);
        }
        kNearestPoints->GetPoint(i, cX, cY);
        //std::cout << "check" << cX << " " << cY << " " << tX << " " << tY << std::endl;
        hull->SetPoint(hull->GetN(), cX, cY);
        
        //prevAngle = getAngle(hull, step, step-1);
        hull->GetPoint(hull->GetN()-2, prevX, prevY); 

        RemovePoint(data, cX, cY);
        step++;
    }
    //std::cout << "check 3" << std::endl;
    // 95% of points?
    bool allInside = !(getPointsEnclosed(hull) <= (0.95 * goodParticles));
    std::cout << getPointsEnclosed(hull) << "/" << goodParticles << std::endl;
    if (!allInside){
        std::cout << "Enclose more points" << std::endl;
        return orderPoints(pointList, k+stepK);
    }
    return hull;
}

double* get99Area(double xMax, double xMin, double yMax, double yMin)
{
    int currPoints = countPointsInBounds(xMax, xMin, yMax, yMin);
    int pointGoal = currPoints;
    double step = 1;
    
    while(pointGoal < countPointsInBounds(xMax, xMin, yMax, yMin))
    {
        xMax -= step;
    }
    xMax += 2*step;
    while(pointGoal < countPointsInBounds(xMax, xMin, yMax, yMin))
    {
        xMin += step;
    }
    xMin -= 2*step;
    while(pointGoal < countPointsInBounds(xMax, xMin, yMax, yMin))
    {
        yMax -= step;
    }
    yMax += 2*step;
    while(pointGoal < countPointsInBounds(xMax, xMin, yMax, yMin))
    {
        yMin += step;
    }
    
    xMax += 8*step;
    xMin -= 8*step;
    yMax += 8*step;
    yMin -= 8*step;
    std::cout << "xMin : " << xMin << std::endl;
    std::cout << "xMax : " << xMax << std::endl;
    std::cout << "yMin : " << yMin << std::endl;
    std::cout << "yMax : " << yMax << std::endl;
    double* res = new double[5];
    res[0] = ((xMax - xMin) * (yMax - yMin));
    res[1] = xMax;
    res[2] = xMin;
    res[3] = yMax;
    res[4] = yMin;
    return res;
}

int main(int argc, char **argv){
    //gROOT->SetBatch(kFALSE);
    if (argc != 2 && argc != 3 && argc != 5 && argc != 8)
    {
        std::cerr << "Usage: ./reader file_name [startZ <stopZ step [startK minArea densityCut]>])" << std::endl;
        exit(0);
    }
    double startZ;
    double stopZ;
    double step;
    double currZ;
    bool noInput = false;
    if (argc == 3)
    {
        noInput = true;
        startZ = atoi(argv[2]); 
        stopZ = startZ; 
        step = 1; 
        currZ = startZ;
    }
    if (argc == 5 || argc == 8)
    {
        noInput = true;
        startZ = atoi(argv[2]); 
        stopZ = atoi(argv[3]); 
        step = atoi(argv[4]); 
        currZ = startZ;
        if (argc == 8)
        {
            startK = atof(argv[5]);
            minArea = atof(argv[6]);
            densityCut = atof(argv[7]);
        }
    }
    
    char* fileName = argv[1];

    goodParticles = 0;
    char ans = 'y'; //default answer is yes
    TPad* pad1;

    while (ans=='y')
    {
        //gROOT->Reset();
        //i = 0;	
        g2 = new TGraph();
        g2->SetFillColor(6);
        if (noInput)
        {
            if (currZ > stopZ)
                ans = 'n';
            else
                ans = 'y';
        }
        else
        {
            std::cout << "\nWould you like to choose vertex points? \t y/n: ";
            std::cin >> ans;
        }

        if (ans=='y')
        {
            if (noInput)
            {
                z_pos = currZ;
                currZ += step;
            }
            else
            {
                std::cout << "What z position would you like to examine?" << std::endl;
                std::cin >> z_pos;
            }
            std::cout << "Starting at z=" << z_pos << std::endl;
            TCanvas* c2 = new TCanvas("c2","points display", 10, 10, 1600, 900);
            pad1 = new TPad("pad1","pad1",.1,.1,.9,.9);
            //pad1->Draw();
            //std::cout << "Counting... ";
            //goodParticles = countTextFile(z_pos, fileName);
            //std::cout << "Done" << std::endl;
            std::cout << "Reading... ";
            particlesX.clear();
            particlesY.clear();

            readTextFile(z_pos, fileName);
            if (particlesX.size() != particlesY.size())
            {
                std::cerr << "X and Y data sizes mismatched" << std::endl;
                exit(1);
            }
            goodParticles = particlesX.size();
            std::cout << "Done" << std::endl;

            std::cout << "Number of points read: " << goodParticles << "\n";
            if (goodParticles >= 1){
                TGraph *particleGraph = new TGraph(goodParticles,&particlesX[0],&particlesY[0]);
                gPad->Modified();
                gPad->Update();
                // do user interactions here:
                if (!gPad) {
                    std::cout << "pad1 is null" << std::endl;
                    //break;
                }	
                //g2->SetPoint(g2->GetN(), -9999, 0);
                double xMax, xMin, yMax, yMin;
                particleGraph->ComputeRange(xMin, yMin, xMax, yMax);
                xMax += 100;
                xMin -= 100;
                yMax += 100;
                yMin -= 100;
                std::cout << "xMax: " << xMax << std::endl;
                std::cout << "xMin: " << xMin << std::endl;
                std::cout << "yMax: " << yMax << std::endl;
                std::cout << "yMin: " << yMin << std::endl;
                
                
                double area = (xMax - xMin) * (yMax - yMin);
                std::cout << "Generating..." << std::endl;

                //double* res = get99Area(xMax, xMin, yMax, yMin);
                //double area = res[0];
                //xMax = res[1];
                //xMin = res[2];
                //yMax = res[3];
                //yMin = res[4];
                yMin = (yMin > -10)? -10 : yMin;

                double heightRatio = (yMax-yMin) / (xMax-xMin);;
                std::cout << "Height to Width: " << heightRatio << std::endl;
                /*double oldMinDensity = densityCut;
                double oldMaxDensity = cut; 
                densityCut = densityCut/area;               
                cut = cut/area;               
                */
                double oldMin = minArea;
                minArea += pow(z_pos/1000.0, 2);//*= (z_pos/100.0-100) * (z_pos/100.0-100); //no overflows;
                //minArea = (minArea > 80)? 80 : minArea;
                maxArea =  maxAreaFactor *minArea;
                cut = 3*goodParticles/area;
                if (cut < 5) 
                {
                    cut = 5;
                    std::cerr << "Too low statistics at this Z and area" << std::endl;
                }
                std::cout << "Area: " << area << std::endl;
                do {
                    delete g2;
                    g2 = new TGraph();
                    //g2->SetPoint(0, 0, 0);
                    //g2->SetPoint(0, xMax, 0);
                    //g2->SetPoint(1, xMin, 0);
                    delete mg;
                    mg = new TMultiGraph();
                    std::cout << "Check points: " << g2->GetN() << std::endl;
                    std::cout << "Parameters: " << std::endl;
                    std::cout << "\tmaxArea: " << maxArea << std::endl;
                    std::cout << "\tminArea: " << minArea << std::endl;
                    std::cout << "\tcritPoints: " << criticalPoints << std::endl;
                    std::cout << "\tstartK: " << startK << std::endl;
                    std::cout << "\tpointLimit: " << pointLimit << std::endl;
                    std::cout << "\tHighDensityCut: " << cut << std::endl;
                    std::cout << "\tLowDensityCut: " << densityCut << std::endl;
                    int cutTimes = ((int)heightRatio)+1;
                    double range = (yMax - yMin)/cutTimes;
                    for (int i = 0; i < cutTimes; i++)
                    {
                        checkQuadrant(xMax, xMin, yMin+(range)*(i+1), yMin+(range)*(i), pointLimit);
                    }
                    std::cout << "Considered points: " << g2->GetN() << std::endl;
                    if (g2->GetN() >= pointLimit)
                    {
                        hull = new TGraph();
                        break;
                    }
                    double minX = 10000000;
                    double maxX = -10000000;
                    for (int i = 0; i < g2->GetN(); i++)
                    {
                        double zX, zY;
                        g2->GetPoint(i, zX, zY);
                        if (zY < 5)
                        {
                            zY = 0;
                            g2->RemovePoint(i--);
                        }
                        //g2->SetPoint(i, zX, zY);
                        if (zY == 0 && zX < minX)
                            minX = zX;
                        if (zY == 0 && zX > maxX)
                            maxX = zX;

                    }
                    double del = sqrt(minArea);
                    for (double i = minX; i < maxX; i+= del)
                        g2->SetPoint(g2->GetN(), i, 0);
                    g2->SetPoint(g2->GetN(), maxX, 0);

                    hull = orderPoints(g2, (g2->GetN() > startK)? startK : g2->GetN());
                    //hull = orderPoints(g2, (g2->GetN() > startK)? startK : g2->GetN());
                    std::cout << "Resultant points: " << hull->GetN() << std::endl;
                     
                }while (false && hull->GetN() <= 10);
                std::cout << "Done! Smoothing..." << std::endl;
                
                //Removing extra across x axis (assume folding)
                int leftIndex = 0;
                int rightIndex = 0;
                double leftX;
                double rightX;
                bool first = true;
                for (int j = 0; j < hull->GetN(); j++)
                {
                    double x, y;
                    hull->GetPoint(j, x, y);
                    
                    if (y <= 6)
                    {
                        hull->SetPoint(j, x, 0);
                        if (first)
                        {
                            //std::cout << "first" << std::endl;
                            first = false;
                            rightIndex = j;
                            leftX = x;
                            rightX = x;
                            leftIndex = j;
                            //std::cout << "l, r: " << leftIndex << ", " << rightIndex << std::endl;
                        }
                        else
                        {
                             if (leftX > x)
                             {
                                leftX = x;
                                leftIndex = j;
                             }
                             if (rightX < x)
                             {
                                rightX = x;
                                rightIndex = j;
                             }
                        }
                    } 
                } 
                for (int j = 0; j < hull->GetN()-1; j++)
                {
                    if (j == leftIndex || j == rightIndex) 
                    {
                        double x, y;
                        hull->GetPoint(j, x, y);
                        //std::cout << "Saving " << j << ": " << x << ", " << y << std::endl;
                        continue;
                    }
                    double x, y;
                    hull->GetPoint(j, x, y);
                    
                    if (y < 0)
                    {
                        //std::cout << "Removing " << j << ": " << x << ", " << y << std::endl;
                        hull->RemovePoint(j--);
                        leftIndex--;
                        rightIndex--;
                    }
                }
                

                //Remove extraneous points
                //Algo: If next 2 form straight line (and exclude no points)
                //remove. else, check next point
                //TODO

                for (int i = 0; i < hull->GetN()-2; i++)
                {
                    double x, y; //start x, y
                    double mx, my, ex, ey; //mid xy, end xy
                    hull->GetPoint(i, x, y);
                    hull->GetPoint(i+1, mx, my);
                    hull->GetPoint(i+2, ex, ey);

                    //make two vectors with m midpoint
                    double vx = x - mx;
                    double vy = y - my;

                    double ux = ex - mx;
                    double uy = ey - my;
                    
                    //dot
                    double dot = vx*ux + vy*uy;
                    //len
                    double lenV = sqrt(vx*vx + vy*vy);
                    double lenU = sqrt(ux*ux + uy*uy);
                    double len = lenV * lenU;
                    //cos theta = dot/len
                    double res = dot/len * dot/len;
                    if (res >= 1-smoothFactor)
                    {
                        std::cout << "Removed: " << mx << ", " << my << " with cos^2(theta) = " << res << std::endl; 
                        hull->RemovePoint(i+1);
                        i--;
                    }
                }



                //Add in phi based points
                //TODO
                
                
                
                //int currK = (startK > g2->GetN() * 2/4)? g2->GetN() * 2/4 : startK;
                //hull = orderPoints(g2, currK);

                //cut = oldMaxDensity;
                //densityCut = oldMinDensity;
                minArea = oldMin;
                maxArea = maxAreaFactor * minArea;
                /*std::cout << "Hull points: " << hull->GetN() << std::endl;
                while (hull->GetN() > 0 && hull->GetN() < maxv)
                {
                    std::cout << "Needs more points!" << std::endl;
                    for (int i = 1; i < hull->GetN()-1; i+=2)
                    {
                        double x, y, ex, ey;
                        hull->GetPoint(i, x, y);
                        hull->GetPoint(i+1, ex, ey);
                        for (int j = hull->GetN(); j > i+1; j--)
                        {
                            double tx, ty;
                            hull->GetPoint(j-1, tx, ty);
                            hull->SetPoint(j, tx, ty);
                        }
                        hull->SetPoint(i+1, (x+ex)/2, (y+ey)/2);
                    }
                    std::cout << "Hull points: " << hull->GetN() << std::endl;
                }
                */
                particleGraph->SetMarkerStyle(6);
                particleGraph->SetMarkerColor(4);
                g2->SetMarkerStyle(6);
                g2->SetMarkerColor(6);
                hull->SetLineColor(2);

                //FIXME
                hull->Draw("AL");
                particleGraph->Draw("AP");
                //g2->Draw("Psame");
                //particleGraph->Draw("Psame");
                hull->Draw("Lsame");

                //g2->SetPoint(g2->GetN(), 9999, 0);

                /*
                   particleGraph->Draw("A");
                   mg->Draw("Fsame");
                   particleGraph->Draw("Psame");
                   g2->Draw("Psame");
                   */
                gPad->Modified();
                gPad->Update();

                /*

                   char ans2 = 'y';
                   while (true) {
                   std::cout << "Do you want to save these " << i << " points?\ty/n: ";
                   cin >> ans2;
                   }
                // DONE
                particleGraph->Delete();
                */
                std::cout << "Done with z = " << z_pos << std::endl;
                TCanvas *imgCanvas = new TCanvas();
                imgCanvas->cd();
                imgCanvas->Divide(4, 1);

                imgCanvas->cd(1);
                hull->Draw("AL");
                particleGraph->Draw("Psame");
                hull->Draw("Lsame");
                g2->Draw("Psame");

                imgCanvas->cd(2);
                g2->Draw("AP");
                particleGraph->Draw("Psame");
                g2->Draw("Psame");

                imgCanvas->cd(3);
                particleGraph->Draw("AP");
                g2->Draw("Psame");

                imgCanvas->cd(4);
                mg->Draw("LFsame"); 

                imgCanvas->Draw();
                TImage *img = TImage::Create();
                img->FromPad(imgCanvas);

                std::string fileString(fileName); 
                int dotPos = fileString.rfind(".");   
                
                std::ostringstream os;
                os << fileString.substr(0, dotPos) << "_" << z_pos << ".png";
                std::string imgName = os.str();
                
                img->WriteImage(imgName.c_str());
                img->WriteImage("test.png");
                
                std::ostringstream vos;
                vos << fileString.substr(0, dotPos) << "_vertex_storage.txt";
                std::string storageName = vos.str();
                
                char filenameO[50];
                std::ofstream outfile;
                sprintf(filenameO, storageName.c_str());
                outfile.open(filenameO,std::ios::app);
                double hullX, hullY;
                for (int k = 0; k < hull->GetN(); k++) {
                    hull->GetPoint(k, hullX, hullY);
                    outfile << hullX << "\t" << hullY << "\t" << z_pos << "\r\n";
                }	
                std::cout << "Stored " << hull->GetN() << " x and y coordinates at z = " << z_pos << std::endl;
                outfile.close();
            }

            else {
                std::cout << "Sorry, the file you have chosen is empty, please try again" << std::endl;
            }
        }
        else if (( ans != 'y' ) && ( ans != 'n' )) {
            std::cout << "Error, please try again using the characters 'y' and 'n'" << std::endl;
            ans = 'y';
        }
    }
    return 0;
}
