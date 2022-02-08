//
//  CollectionCells.swift
//  mo-nomz
//
//  Created by Dan Fithian on 2/3/22.
//

import UIKit

class OneCellButton: UICollectionViewCell {
    @IBOutlet weak var button: UIButton!
}

class OneCellImage: UICollectionViewCell {
    @IBOutlet weak var image: UIImageView!
}

class CollectionHeader: UICollectionReusableView {
    @IBOutlet weak var label: UILabel!
}
