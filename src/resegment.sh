#!/bin/bash

DIR=/cis/home/epostell/Projects/def_scz/cut_pt_surfaces

FILE=list.txt

ROI=lh

cat list.txt | while read LIST

do

cd ${DIR}/${LIST}

cp "$DIR"/${LIST}/"$ROI"_PT_surf_child_0.5.byu "$DIR"/${LIST}/current_roi_cut.byu

cp "$DIR"/${LIST}/"$ROI"_stg_surf_child_0.5.byu "$DIR"/${LIST}/current_roi.byu

cp "$DIR"/${LIST}/segment.img "$DIR"/${LIST}/current_roi_seg.img
cp "$DIR"/${LIST}/segment.hdr "$DIR"/${LIST}/current_roi_seg.hdr

bwtest -s SSD.bws

createDepth `read_hdr "$DIR"/"${LIST}"/current_roi_seg.hdr | grep width | awk '{print $6}'` `read_hdr "$DIR"/"${LIST}"/current_roi_seg.hdr | grep height | awk '{print $6}'` `read_hdr "$DIR"/"${LIST}"/current_roi_seg.hdr | grep depth | awk '{print $6}'` current_roi_cut.byu current_child.byu current_roi_seg current_roi_bin2 25 125

make_hdr current_roi_bin2.hdr `read_hdr "$DIR"/"${LIST}"/current_roi_seg.hdr | grep width | awk '{print $6}'` `read_hdr "$DIR"/"${LIST}"/current_roi_seg.hdr | grep height | awk '{print $6}'` `read_hdr "$DIR"/"${LIST}"/current_roi_seg.hdr | grep depth | awk '{print $6}'` 0.5 CHAR 255 1 1 1 1 

image_algebra current_roi_bin2 x current_roi_seg current_roi_seg2

cp "$DIR"/${LIST}/current_roi_seg2.img "$DIR"/${LIST}/"$ROI"_PT_0.5_segment_0.5.img
cp "$DIR"/${LIST}/segment.hdr "$DIR"/${LIST}/"$ROI"_PT_0.5_segment_0.5.hdr


done


