/* ************************************************************************** */
/*                                                                            */
/*                                  interp.h                                  */
/*                                                                            */
/* ************************************************************************** */

#ifndef __interp__
#define __interp__

/* -------------------------------------------------------------------------- */
/*                                                                            */
/* RCS id string                                                              */
/*   "$Id: interp.h,v 1.1 2001/10/04 17:55:22 graziano Exp $";
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
/* FILENAME   : interp.h                                                      */
/* AUTHOR     : Graziano Giuliani                                             */
/* DESCRIPTION: Interpolation routines header file                            */
/* DOCUMENTS  :                                                               */
/*                                                                            */
/* ************************************************************************** */
 
/* -------------------------------------------------------------------------- */
/*  DEFINITIONS                                                               */
/* -------------------------------------------------------------------------- */
#define INT_NEARNEIGH 0
#define INT_DISTWEGHT 1
#define INT_BILINEAR  2
#define INT_CUBIC     3

#define IS_CROSS 1

/* -------------------------------------------------------------------------- */
/*  EXPORTED FUNCTION PROTOTYPES                                              */
/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */
/* FUNCTION    : interpolate                                                  */
/* DESCRIPTION : Interpolates from uniform grid points using chosen mode      */
/* IN DATA     : gridded points array and its dimensions (ii * jj)            */
/*               latitude and longitude of point to interpolate               */
/*               cross-dot flag                                               */
/* OUT DATA    : none                                                         */
/* RETURN      : Interpolated value of field at latp, lonp                    */
/* -------------------------------------------------------------------------- */
extern float interpolate(float *field, int ii, int jj, float latp, float lonp,
                         float missing_value, int inttype, int cdflag);

#endif /* __interp__ */
