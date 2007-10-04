/* ************************************************************************** */
/*                                                                            */
/*                               map_proj.h                                   */
/*                                                                            */
/* ************************************************************************** */
 
#ifndef __map_proj__
#define __map_proj__

/* -------------------------------------------------------------------------- */
/*                                                                            */
/* RCS id string                                                              */
/*   "$Id: map_proj.h,v 1.1 2001/10/04 17:55:55 graziano Exp $";
                                                                              */
/*                                                                            */
/* -------------------------------------------------------------------------- */
 
/* ************************************************************************** */
/* ***********************  UNIVERSITA' DEGLI STUDI  ************************ */
/* ******************************   L'AQUILA   ****************************** */
/* ************************************************************************** */
/*                                                                            */
/*                   Dipartimento di Fisica dell'Atmosfera                    */
/*                                                                            */
/* PROJECT    : PSTd'A 2001                                                   */
/* FILENAME   : map_proj.h                                                    */
/* AUTHOR     : Graziano Giuliani                                             */
/* DESCRIPTION: Map projection utilities. Support Lambert, Polar and Mercator */
/* DOCUMENTS  :                                                               */
/*                                                                            */
/* ************************************************************************** */

/* -------------------------------------------------------------------------- */
/*  DEFINITIONS                                                               */
/* -------------------------------------------------------------------------- */
#define DEG_TO_RAD      0.01745329251994329576
#define RAD_TO_DEG      57.29577951308232088523
#define RADIUS_EARTH    6370.0
 
#define LAMBERT  1
#define POLAR    2
#define MERCATOR 3

/* -------------------------------------------------------------------------- */
/*  DEFINITIONS                                                               */
/* -------------------------------------------------------------------------- */

typedef struct {
  int    code;
  int    c_dim_i;
  int    c_dim_j;
  int    dim_i;
  int    dim_j;
  int    exp_flag;
  double ratio;
  double stdlat1;
  double stdlat2;
  double stdlon;
  double cenlat;
  double cenlon;
  double ds;
  double cds;
  double xn;
  double xsouth;
  double xwest;
  double exp_off_i;
  double exp_off_j;
} proj_info;
 
/* -------------------------------------------------------------------------- */
/*  EXPORTED FUNCTION PROTOTYPES                                              */
/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */
/* FUNCTION : proj_init                                                       */
/* DESCRIPT : Initialises common parameters for projection routines           */
/* IN DATA  : proj_info structure                                             */
/* OUT DATA : none                                                            */
/* RETURN   : none                                                            */
/* -------------------------------------------------------------------------- */
extern void proj_init(proj_info *in_pj);

/* -------------------------------------------------------------------------- */
/* FUNCTION : proj_end                                                        */
/* DESCRIPT : Terminate projection routines freeing up space                  */
/* IN DATA  : none                                                            */
/* OUT DATA : none                                                            */
/* RETURN   : none                                                            */
/* -------------------------------------------------------------------------- */
extern void proj_end();

/* -------------------------------------------------------------------------- */
/* FUNCTION : latlon_to_ij                                                    */
/* DESCRIPT : Transforms a latitude longitude pair into a i j pair on grid    */
/* IN DATA  : latitude-longitude pair                                         */
/* OUT DATA : ri-rj coordinate pair on grid                                   */
/* RETURN   : none                                                            */
/* -------------------------------------------------------------------------- */
extern void latlon_to_ij(float latitude, float longitude,
                         float *ri, float *rj);

/* -------------------------------------------------------------------------- */
/* FUNCTION : ij_to_latlon                                                    */
/* DESCRIPT : Transforms an i j pair on grid on latitude-longitude pair       */
/* IN DATA  : ri-rj coordinate pair on grid                                   */
/* OUT DATA : latitude-longitude pair                                         */
/* RETURN   : none                                                            */
/* -------------------------------------------------------------------------- */
extern void ij_to_latlon(float ri, float rj,
                         float *latitude, float *longitude);

#endif /* __map_proj__ */
