//
//  RecipeFilterController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 3/6/22.
//

import UIKit

protocol RecipeFilterDelegate {
    func updateTags(active: Bool, tags: Set<String>) -> [String]
}

class RecipeFilterController: UICollectionViewController, UICollectionViewDelegateFlowLayout {
    var delegate: RecipeFilterDelegate? = nil
    var active: Bool = true
    var tags: [String]? = nil
    var selected: Set<String> = Set<String>()
    
    let ACTIVE = 0
    let TAGS = 1
    let CLEAR = 2

    override func numberOfSections(in collectionView: UICollectionView) -> Int {
        return 3
    }
    
    override func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        switch section {
        case ACTIVE: return 1
        case TAGS: return tags?.count ?? 0
        case CLEAR: return 1
        default: return 0
        }
    }
    
    private func sendUpdates() {
        let newTags = delegate?.updateTags(active: active, tags: selected) ?? []
        tags = newTags
        selected = selected.filter({ (selected_) in !newTags.filter({ (tag_) in tag_ == selected_ }).isEmpty })
        collectionView.reloadData()
    }
    
    @objc func didTapClear(_ sender: Any?) {
        active = true
        selected = Set<String>()
        sendUpdates()
    }
    
    @objc func didTapActive(_ sender: Any?) {
        active = !active
        sendUpdates()
    }
    
    private func toggleTag(_ index: Int) {
        let tag_ = tags![index]
        if selected.contains(tag_) {
            selected.remove(tag_)
        } else {
            selected.insert(tag_)
        }
        sendUpdates()
    }
    
    @objc func didTapTag(_ sender: Any?) {
        guard let button = sender as? UIButton else { return }
        toggleTag(button.tag)
    }
    
    override func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        switch indexPath.section {
        case ACTIVE:
            let cell = collectionView.dequeueReusableCell(withReuseIdentifier: "activeButton", for: indexPath) as! OneCellButton
            cell.button.setTitle("active", for: .normal)
            cell.button.layer.cornerRadius = 5
            if active {
                cell.button.backgroundColor = UIColor.systemGray5
            } else {
                cell.button.backgroundColor = nil
            }
            cell.button.addTarget(self, action: #selector(didTapActive), for: .touchUpInside)
            return cell
        case TAGS:
            let cell = collectionView.dequeueReusableCell(withReuseIdentifier: "tagButton", for: indexPath) as! OneCellButton
            let tag_ = tags![indexPath.row]
            cell.button.tag = indexPath.row
            cell.button.setTitle(tag_, for: .normal)
            cell.button.layer.cornerRadius = 5
            if selected.contains(tag_) {
                cell.button.backgroundColor = UIColor.systemGray5
            } else {
                cell.button.backgroundColor = nil
            }
            cell.button.addTarget(self, action: #selector(didTapTag), for: .touchUpInside)
            return cell
        case CLEAR:
            let cell = collectionView.dequeueReusableCell(withReuseIdentifier: "clearButton", for: indexPath) as! OneCellButton
            cell.button.addTarget(self, action: #selector(didTapClear), for: .touchUpInside)
            return cell
        default:
            return UICollectionViewCell()
        }
    }
    
    func collectionView(_ collectionView: UICollectionView, layout collectionViewLayout: UICollectionViewLayout, sizeForItemAt indexPath: IndexPath) -> CGSize {
        let text: String
        switch indexPath.section {
        case ACTIVE:
            text = "active"
            break
        case TAGS:
            text = tags?[indexPath.row] ?? ""
            break
        default:
            text = ""
            break
        }
        let font = UIFont.systemFont(ofSize: 12)
        let fontAttributes = [NSAttributedString.Key.font: font]
        let buttonWidth = 25.0
        let width = (text as NSString).size(withAttributes: fontAttributes as [NSAttributedString.Key : Any]).width + buttonWidth
        let height = view.frame.size.height - 10
        return CGSize(width: width, height: height)
    }
}
