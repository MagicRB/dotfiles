;;
;; $Id: svg.el,v f0c325fe1142 2016/04/16 12:48:31 pkgs $
;;


(defun hmw/svg-header (width height)
  (insert (format "<?xml version=\"1.0\"?>
<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"%s\" height=\"%s\" >
  <g style=\"fill-opacity:1.0; stroke:black; stroke-width:1;\">" width height)))


(defun hmw/svg-footer ()
  (insert "\n  </g>\n</svg>"))


(defun hmw/svg-text(x y str)
  (insert (format "
    <text x=\"%s\" y=\"%s\"
          font-size=\"10.000000\"
          fill=\"#000000\"
          font-family=\"Helvetica\"
          font-weight=\"normal\"
          text-anchor=\"start\"
          stroke=\"none\">
    %s
    </text>" x y str)))


(defun hmw/svg-line(x1 y1 x2 y2)
  (insert (format "
    <line x1=\"%s\" y1=\"%s\"
          x2=\"%s\" y2=\"%s\"
          style=\"stroke:#000000;stroke-width:1.000000\"/>" x1 y1 x2 y2)))


(defun hmw/svg-rect(x y width height col)
  (insert (format "
    <rect x=\"%s\" y=\"%s\"
          height=\"%s\" width=\"%s\"
          style=\"fill:%s;stroke:none\"/>" x y height width col)))

(provide 'org-task-svg)
