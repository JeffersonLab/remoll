#include <stdio.h>

#include "HDDM/hddm_s.h"

s_iostream_t* hddmOutputStream=NULL;

void bg_getvertex_(float myvertex[3]);

typedef struct {
        int geantid;
        int mech; /* what do the values of this correspond to */
        int kfid;
        int parent;
        int firstdaughter;
        int lastdaughter;
} keve_t;

typedef struct {
   float px;
   float py;
   float pz;
   float en;
} peve_t;

/*-----------------
// open_hddm_output_
//-----------------*/
void open_hddm_output_(const char *outputfile, int len)
{
   /* Copy FORTRAN string into a C-style string */
   char outfile[256];
   strncpy(outfile, outputfile, len);
   outfile[len]=0;

   /* Open output file */
   hddmOutputStream = init_s_HDDM(outfile);
   if (! hddmOutputStream) {
      fprintf(stderr, "Unable to open output file \"%s\" for writing.\n", outfile);
      exit(-3);
   }
   
   printf("Opened HDDM file \"%s\" for writing ...\n", outfile);
}

/*-----------------
// close_hddm_output_
//-----------------*/
void close_hddm_output_(void)
{
   /* Close output file */
   close_s_HDDM(hddmOutputStream);
   
   printf("Closed HDDM output file\n");
}

/*-----------------
// write_hddm_event_
//-----------------*/
void write_hddm_event_(int *runno, int *iev, int *iproc,
                       keve_t *kin,  peve_t *pin,   
            int *ntra, keve_t *keve, peve_t *peve)
{
   /* Loop over events */
   int i;
   static int Nevents = 0;
   static int Nevents_written = 0;
   int runNumber = *runno;
   float vertex[3]={0.0, 0.0, 0.0}; 

   Nevents++;

   /* Start a new event */
   s_PhysicsEvents_t* pes;
   s_Reactions_t* rs;
   s_Beam_t* bs;
   s_Momentum_t *mom;
   s_Properties_t *prop;
   s_Target_t* ts;
   s_Vertices_t* vs;
   s_Origin_t* origin;
   s_Products_t* ps;

   s_HDDM_t *thisOutputEvent = make_s_HDDM();
   thisOutputEvent->physicsEvents = pes = make_s_PhysicsEvents(1);
   pes->mult = 1;
   pes->in[0].runNo   = runNumber;
   pes->in[0].eventNo = Nevents;
   pes->in[0].reactions = rs = make_s_Reactions(1);
   rs->mult = 1;
   rs->in[0].type = *iproc;

   rs->in[0].beam = bs = make_s_Beam();
        bs->type = kin[0].geantid;
        bs->momentum = mom = make_s_Momentum();
        mom->px = pin[0].px;
        mom->py = pin[0].py;
        mom->pz = pin[0].pz;
        mom->E  = pin[0].en;
        bs->properties = prop = make_s_Properties();
        prop->charge = 0.0;
        prop->mass = 0.0;
        
   rs->in[0].target = ts = make_s_Target();
        ts->type = kin[1].geantid;
        ts->momentum = mom = make_s_Momentum();
        mom->px = pin[1].px;
        mom->py = pin[1].py;
        mom->pz = pin[1].pz;
        mom->E  = pin[1].en;
        ts->properties = prop = make_s_Properties();
        prop->charge = +1;
        prop->mass = 0.938272; /* this should be derived from type ... */
        
   rs->in[0].vertices = vs = make_s_Vertices(1);
   vs->mult = 1;
   vs->in[0].origin = origin = make_s_Origin();
   vs->in[0].products = ps = make_s_Products(*ntra);
   ps->mult = 0;

   // Copy vertex values from FORTRAN common block
   bg_getvertex_(vertex);
   
   origin->t = 0.0;
   origin->vx = vertex[0];
   origin->vy = vertex[1];
   origin->vz = vertex[2];
   
   for (i=0; i < *ntra; i++) {
      /* double E2;  unused so commented out 12/18/2013 DL */
      //if(keve[i].geantid==0)continue;
      
      ps->in[ps->mult].type = keve[i].geantid;
                ps->in[ps->mult].mech = keve[i].mech;
                ps->in[ps->mult].pdgtype = keve[i].kfid;
      ps->in[ps->mult].id = i+1;
                ps->in[ps->mult].parentid = keve[i].parent;

      
      ps->in[ps->mult].momentum = make_s_Momentum();
      ps->in[ps->mult].momentum->px = peve[i].px;
      ps->in[ps->mult].momentum->py = peve[i].py;
      ps->in[ps->mult].momentum->pz = peve[i].pz;
      ps->in[ps->mult].momentum->E  = peve[i].en;
      ps->mult++;
   }
   
   if ( *ntra > 0) {
      Nevents_written++;
      if (flush_s_HDDM(thisOutputEvent, hddmOutputStream) != 0) {
         fprintf(stderr,"Error - write failed to output hddm file "
                 "after %d events were written.\n", Nevents_written);
         exit(2);
      }
      if (Nevents_written%10000 == 0)
         printf("Wrote event %d events (%d generated)\n",
                Nevents_written, Nevents);
   }   
}
